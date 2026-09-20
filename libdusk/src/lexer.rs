use std::ffi::CString;
use std::ops::Range;

use unicode_segmentation::GraphemeCursor;

use crate::source_info::{SourceFileId, SourceRange};

use crate::driver::Driver;
use crate::token::{TokenKind, TokenVec};
use crate::error::Error;

struct Lexer<'src> {
    /// Byte offset of the current file in the global source map
    file_offset: usize,

    /// Start of current grapheme (relative to file_offset)
    start: usize,
    /// End of current grapheme (relative to file_offset)
    end: GraphemeCursor,

    tok_start_loc: usize,

    file_id: SourceFileId,
    src: &'src str,
}

impl Driver {
    pub fn lex(&self, file: SourceFileId) -> Result<TokenVec, ()> {
        let mut l = Lexer::new(file, self);
        l.lex(self)
    }
}

impl<'src> Lexer<'src> {
    fn new(file_id: SourceFileId, d: &'src Driver) -> Self {
        let file = &d.src_map.files[file_id];
        Self {
            file_offset: file.begin_offset,
            start: 0,
            end:   GraphemeCursor::new(0, file.src.len(), true),
            tok_start_loc: 0,
            src: &file.src,
            file_id,
        }
    }

    fn lex(&mut self, d: &Driver) -> Result<TokenVec, ()> {
        self.next_boundary();

        // Find line breaks.
        let mut lines = vec![0];
        while self.has_chars() {
            if self.is(b'\n') {
                self.advance();
                lines.push(self.cur_loc());
            } else if self.is_str(b"\r\n") {
                unsafe { self.advance_by_ascii(2); }
                lines.push(self.cur_loc());
            } else if self.is(b'\r') {
                self.advance();
                lines.push(self.cur_loc());
            } else {
                self.advance();
            }
        }
        d.src_map.files[self.file_id].lines.set(lines).unwrap();

        self.set_pos(0);
        let mut toks = TokenVec::default();
        loop {
            let Ok((tok, range)) = self.next(d) else {
                return Err(());
            };
            let should_break = tok == TokenKind::Eof;
            toks.push(tok, range);
            if should_break { break; }
        }
        Ok(toks)
    }

    fn cur_loc(&self) -> usize {
        self.start
    }

    fn has_chars(&self) -> bool {
        self.start < self.end.cur_cursor()
    }

    fn make_src_range(&self, range: Range<usize>) -> SourceRange {
        SourceRange {
            start: range.start + self.file_offset,
            end: range.end + self.file_offset,
        }
    }

    fn cur_grapheme(&self) -> &str {
        &self.src[self.start..self.end.cur_cursor()]
    }

    /// Calls `self.end.next_boundary()`, with a fast-path for ASCII
    fn next_boundary(&mut self) {
        self.next_boundary_from(self.end.cur_cursor());
    }

    fn next_boundary_from(&mut self, start: usize) {
        if start == self.src.len() { return; }

        let cur_byte = self.src.as_bytes()[start];
        if cur_byte & 0x80 == 0 {
            let next_cursor = std::cmp::min(start + 1, self.src.len());
            self.end.set_cursor(next_cursor);
        } else {
            self.end.next_boundary(self.src, 0).unwrap();
        }
    }

    fn set_pos(&mut self, pos: usize) {
        self.start = pos;
        self.next_boundary_from(pos);
    }

    /// Skip to next grapheme.
    fn advance(&mut self) {
        self.start = self.end.cur_cursor();
        self.next_boundary();
    }

    fn is(&self, character: u8) -> bool {
        if self.has_chars() {
            self.src.as_bytes()[self.start] == character
        } else {
            false
        }
    }

    fn is_str(&self, slice: &[u8]) -> bool {
        if slice.len() > self.src.len() - self.start {
            return false;
        }
        for (a, b) in self.src.as_bytes()[self.start..].iter().zip(slice.iter()) {
            if a != b { return false; }
        }
        true
    }

    /// Skip over `n` bytes of known-ASCII text
    unsafe fn advance_by_ascii(&mut self, n: usize) {
        self.start = std::cmp::min(self.start + n, self.src.len());
        self.next_boundary_from(self.end.cur_cursor() + n - 1);
    }

    fn is_letter(&self) -> bool {
        let mut chars = self.cur_grapheme().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_alphabetic()
        } else {
            false
        }
    }
    fn is_hex_digit(&self) -> bool {
        let mut chars = self.cur_grapheme().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_ascii_hexdigit()
        } else {
            false
        }
    }
    fn is_newline(&self) -> bool { self.is(b'\n') || self.is(b'\r') || self.is_str(b"\r\n") }
    fn is_whitespace(&self) -> bool { self.is(b' ') || self.is(b'\t') }
    fn is_num(&self) -> bool {
        let mut chars = self.cur_grapheme().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_ascii_digit()
        } else {
            false
        }
    }

    fn pack_tok(&mut self, kind: TokenKind) -> (TokenKind, SourceRange) {
        let range = self.tok_start_loc..self.cur_loc();
        let range = self.make_src_range(range);
        self.tok_start_loc = self.cur_loc();
        (kind, range)
    }

    fn diagnose_cpp_style_digit_separator(&self, d: &Driver) {
        d.diag.report_error("C++14-style digit separators are not supported in Dusk", self.make_src_range(self.cur_loc()..(self.cur_loc() + 1)), "hint: replace ' with _");
    }

    fn next(&mut self, d: &Driver) -> Result<(TokenKind, SourceRange), ()> {
        if !self.has_chars() {
            Ok(self.pack_tok(TokenKind::Eof))
        } else if self.is_newline() {
            while self.has_chars() && self.is_newline() {
                self.advance();
            }
            Ok(self.pack_tok(TokenKind::Newline))
        } else if self.is_whitespace() {
            while self.has_chars() && self.is_whitespace() {
                self.advance();
            }
            Ok(self.pack_tok(TokenKind::Whitespace))
        } else if self.is_str(b"//") {
            unsafe { self.advance_by_ascii(2); }
            while self.has_chars() && !self.is_newline() {
                self.advance();
            }
            Ok(self.pack_tok(TokenKind::SingleLineComment))
        } else if self.is_str(b"/*") {
            let mut comment_begin = self.cur_loc();
            let mut levels = 1;
            unsafe { self.advance_by_ascii(2); }
            let mut prev_ending_delimiter = None;
            while self.has_chars() {
                if self.is_str(b"/*") {
                    levels += 1;
                    comment_begin = self.cur_loc();
                    self.advance();
                } else if self.is_str(b"*/") {
                    prev_ending_delimiter = Some(self.cur_loc());
                    levels -= 1;
                    self.advance();
                    assert!(levels >= 0);
                    if levels == 0 {
                        self.advance();
                        break;
                    }
                }
                self.advance();
            }
            if levels > 0 {
                let range = self.make_src_range(comment_begin..(comment_begin+2));
                let mut err = Error::new(
                    "unterminated '/*' comment"
                ).adding_primary_range(
                    range,
                    "previous '/*' delimiter here"
                );
                if let Some(prev_ending_delimiter) = prev_ending_delimiter {
                    let range = self.make_src_range(
                        prev_ending_delimiter..(prev_ending_delimiter + 2)
                    );
                    err.add_primary_range(range, "previous '*/' delimiter here");
                    // Reset the position to the position right after the previous ending delimiter so we can keep lexing.
                    // This might be a terrible idea, we'll just have to wait and see.
                    self.set_pos(prev_ending_delimiter + 2);
                }
                d.diag.push(err);
            }
            Ok(self.pack_tok(TokenKind::MultiLineComment))
        } else if self.is_str(b"*/") {
            unsafe { self.advance_by_ascii(2); }
            let range = self.make_src_range(self.tok_start_loc..self.cur_loc());
            d.diag.push(
                Error::new(
                    "unexpected '*/' delimiter"
                ).adding_primary_range(
                    range,
                    "no previous '/*' to match"
                )
            );
            self.next(d)
        } else if self.is(b'"') {
            self.advance();
            let mut in_escape_mode = false;
            let mut lit = String::new();
            let mut terminated = false;
            while self.has_chars() && !self.is_newline() {
                let char_to_insert = if in_escape_mode {
                    match self.cur_grapheme() {
                        "n" => "\n",
                        "\"" => "\"",
                        "0" => "\0",
                        "\\" => "\\",
                        _ => {
                            let range = self.make_src_range(self.cur_loc()..(self.cur_loc() + 1));
                            d.diag.push(
                                Error::new(
                                    format!("invalid escape character '{}'", self.cur_grapheme())
                                ).adding_primary_range(
                                    range,
                                    "escaped here"
                                )
                            );
                            self.cur_grapheme()
                        }
                    }
                } else {
                    self.cur_grapheme()
                };

                match char_to_insert {
                    "\\" => {
                        self.advance();
                        in_escape_mode = true;
                    },
                    "\"" if !in_escape_mode => {
                        self.advance();
                        terminated = true;
                        break;
                    },
                    char_to_insert => {
                        lit += char_to_insert;
                        self.advance();
                        in_escape_mode = false;
                    }
                }
            }
            if !terminated {
                let msg = if lit.len() == 1 {
                    "unterminated character literal"
                } else {
                    "unterminated string literal"
                };
                let range = self.make_src_range(self.tok_start_loc..self.tok_start_loc + 1);
                d.diag.push(
                    Error::new(
                        msg
                    ).adding_primary_range(
                        range,
                        "literal begins here"
                    )
                );
            }
            if lit.len() == 1 {
                Ok(self.pack_tok(TokenKind::CharLit(lit.as_bytes()[0] as i8)))
            } else {
                Ok(self.pack_tok(TokenKind::StrLit(CString::new(lit).unwrap())))
            }
        } else if self.has_chars() && (self.is_letter() || self.is(b'_')) {
            let ident_start = self.cur_loc();
            let ident_end;
            loop {
                self.advance();

                if !self.has_chars() || (!self.is_letter() && !self.is(b'_') && !self.is_num()) {
                    ident_end = self.cur_loc();
                    break;
                }
            }

            let ident_bytes = &self.src.as_bytes()[ident_start..ident_end];
            let ident = unsafe { std::str::from_utf8_unchecked(ident_bytes) };

            // Add keywords
            use TokenKind::*;
            let kind: TokenKind = match ident {
                "fn" => Fn,
                "return" => Return,
                "true" => True,
                "false" => False,
                "if" => If,
                "else" => Else,
                "while" => While,
                "break" => Break,
                "continue" => Continue,
                "for" => For,
                "in" => In,
                "switch" => Switch,
                "as" => As,
                "struct" => Struct,
                "enum" => Enum,
                "do" => Do,
                "mut" => Mut,
                "mod" => Module,
                "extend" => Extend,
                "extern_mod" => ExternModule,
                "_debug_mark" => DebugMark,
                _ => {
                    let ident = d.interner.write().unwrap().get_or_intern(ident);
                    Ident(ident)
                },
            };
            Ok(self.pack_tok(kind))
        } else if self.has_chars() && self.is_num() {
            let mut has_dot = false;
            let mut last_was_dot = None;
            let mut numeric_chars = String::new();
            let mut has_cpp_style_digit_separator = false;
            while self.has_chars() {
                let is_num = self.is_num();
                let is_dot = self.is(b'.');
                let is_single_quote = self.is(b'\'');
                let is_underscore = self.is(b'_');
                if !(is_num || is_dot || is_single_quote || is_underscore) {
                    break;
                }

                if is_num {
                    numeric_chars.push_str(self.cur_grapheme());
                    last_was_dot = None;
                } else if is_dot {
                    // A decimal literal may have a maximum of one '.'
                    if has_dot {
                        break;
                    } else {
                        has_dot = true;
                        numeric_chars.push_str(self.cur_grapheme());
                    }
                    last_was_dot = Some(self.cur_loc());
                } else {
                    if is_single_quote && !has_cpp_style_digit_separator {
                        has_cpp_style_digit_separator = true;
                        self.diagnose_cpp_style_digit_separator(d);
                    }
                    last_was_dot = None;
                }
                self.advance();
            }

            // If the last character was a dot, it is not part of the literal and should therefore be ignored
            if let Some(dot_pos) = last_was_dot {
                numeric_chars.pop();
                self.set_pos(dot_pos);
                has_dot = false;
            }
            if has_dot {
                Ok(self.pack_tok(TokenKind::DecLit(numeric_chars.parse().unwrap())))
            } else {
                Ok(self.pack_tok(TokenKind::IntLit(numeric_chars.parse().unwrap())))
            }
        } else if self.is(b'$') {
            self.tok_start_loc = self.cur_loc();
            let dollar_sign_location = self.cur_loc();

            // Ignore the dollar sign
            self.advance();


            let mut numeric_chars = String::new();
            let mut has_cpp_style_digit_separator = false;
            while self.has_chars() {
                let is_hex = self.is_hex_digit();
                let is_underscore = self.is(b'_');
                let is_single_quote = self.is(b'\'');
                if !(is_hex || is_underscore || is_single_quote) {
                    break;
                }

                if is_hex {
                    numeric_chars.push_str(self.cur_grapheme());
                } else if is_single_quote && !has_cpp_style_digit_separator {
                    has_cpp_style_digit_separator = true;
                    self.diagnose_cpp_style_digit_separator(d);
                }
                self.advance();
            }
            if numeric_chars.is_empty() {
                let location = if self.has_chars() {
                    self.cur_loc()
                } else {
                    dollar_sign_location
                };
                let range = self.make_src_range(location..(location + 1));
                d.diag.report_error_no_range_msg("expected hex digit after $", range);
                Ok(self.pack_tok(TokenKind::IntLit(0)))
            } else {
                Ok(self.pack_tok(TokenKind::IntLit(u64::from_str_radix(&numeric_chars, 16).unwrap())))
            }
        } else {
            macro_rules! match_symbols {
                ($($kind: ident $symbol: expr)+) => {
                    if false { unreachable!() }
                    $(
                        else if self.is_str($symbol) {
                            // symbols are all ASCII, so it's safe to assume that number of grapheme clusters == number of bytes
                            unsafe { self.advance_by_ascii($symbol.len()); }
                            Ok(self.pack_tok(TokenKind::$kind))
                        }
                    )+
                    else {
                        let range = self.make_src_range(self.tok_start_loc..(self.cur_loc() + 1));
                        d.diag.push(
                            Error::new(
                                format!("unrecognized token '{}'", self.cur_grapheme())
                            ).adding_primary_range(
                                range,
                                ""
                            )
                        );
                        Err(())
                    }
                }
            }
            match_symbols!(
                Colon               b":"
                Semicolon           b";"
                Comma               b","
                LeftParen           b"("
                RightParen          b")"
                DoubleDot           b".."
                Dot                 b"."
                OpenCurly           b"{"
                CloseCurly          b"}"
                OpenSquareBracket   b"["
                CloseSquareBracket  b"]"
                OpenGenerics        b"<|"
                CloseGenerics       b"|>"
                ReturnArrow         b"->"

                Tilde               b"~"
                AddAssign           b"+="
                SubAssign           b"-="
                MultAssign          b"*="
                DivAssign           b"/="
                ModAssign           b"%="
                BitwiseOrAssign     b"|="
                BitwiseAndAssign    b"&="
                XorAssign           b"^="
                LeftShiftAssign     b"<<="
                RightShiftAssign    b">>="
                Caret               b"^"
                LeftShift           b"<<"
                RightShift          b">>"
                Add                 b"+"
                Sub                 b"-"
                Asterisk            b"*"
                Div                 b"/"
                Mod                 b"%"
                Equal               b"=="
                NotEqual            b"!="
                Lte                 b"<="
                Lt                  b"<"
                Gte                 b">="
                GT                  b">"
                LogicalOr           b"||"
                LogicalAnd          b"&&"
                LogicalNot          b"!"
                Assign              b"="
                Ampersand           b"&"
                Pipe                b"|"
                AtSign              b"@"
            )
        }
    }
}
