
use std::cmp::max;
use std::collections::HashMap;
use std::ffi::CString;
use std::ops::Range;

use unicode_segmentation::GraphemeCursor;

use dusk_dire::source_info::{SourceRange, SourceFileId};

use crate::driver::Driver;
use crate::token::TokenKind;
use crate::error::Error;
use crate::source_info::SourceFile;

struct Lexer {
    /// Byte offset of the current file in the global source map
    file_offset: usize,

    /// Start of current grapheme (relative to file_offset)
    start: usize,
    /// End of current grapheme (relative to file_offset)
    end: GraphemeCursor,

    tok_start_loc: usize,

    special_escape_characters: HashMap<&'static str, &'static str>,

    file: SourceFileId,
}

impl Lexer {
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
}

impl Driver {
    pub fn lex(&mut self, file: SourceFileId) -> Result<(), ()> {
        let special_escape_characters = {
            let mut map = HashMap::new();
            map.insert("n", "\n");
            map.insert("\"", "\"");
            map.insert("0", "\0");
            map.insert("\\", "\\");
            map
        };

        let f = &self.src_map.files[file];
        let file_offset = self.src_map.get_begin_offset(file);

        let mut l = Lexer {
            file_offset,
            start: 0,
            end:   GraphemeCursor::new(0, f.src.len(), true),
            tok_start_loc: 0,
            special_escape_characters,
            file,
        };
        self.next_boundary(&mut l);

        // Find line breaks.
        while l.has_chars() {
            if self.is(&l, b'\n') {
                self.advance(&mut l);
                self.src_map.files[file].lines.push(l.cur_loc());
            } else if self.is_str(&l, b"\r\n") {
                unsafe { self.advance_by_ascii(&mut l, 2); }
                self.src_map.files[file].lines.push(l.cur_loc());
            } else if self.is(&l, b'\r') {
                self.advance(&mut l);
                self.src_map.files[file].lines.push(l.cur_loc());
            } else {
                self.advance(&mut l);
            }
        }
        self.set_pos(&mut l, 0);
        self.toks.resize_with(max(self.toks.len(), file.index() + 1), Default::default);
        loop {
            let (tok, range) = if let Ok(next) = self.l_next(&mut l) {
                next
            } else {
                return Err(());
            };
            let should_break = tok == TokenKind::Eof;
            self.toks[file].push(tok, range);
            if should_break { break; }
        }

        Ok(())
    }

    fn file(&self, l: &Lexer) -> &SourceFile {
        &self.src_map.files[l.file]
    }

    fn cur_grapheme(&self, l: &Lexer) -> &str {
        &self.file(l).src[l.start..l.end.cur_cursor()]
    }

    /// Calls `self.end.next_boundary()`, with a fast-path for ASCII
    fn next_boundary(&mut self, l: &mut Lexer) {
        self.next_boundary_from(l, l.end.cur_cursor());
    }

    fn next_boundary_from(&mut self, l: &mut Lexer, start: usize) {
        if start == self.file(l).src.len() { return; }

        let cur_byte = self.file(l).src.as_bytes()[start];
        if cur_byte & 0x80 == 0 {
            let next_cursor = std::cmp::min(start + 1, self.file(l).src.len());
            l.end.set_cursor(next_cursor);
        } else {
            l.end.next_boundary(&self.file(l).src, 0).unwrap();
        }
    }

    fn set_pos(&mut self, l: &mut Lexer, pos: usize) {
        l.start = pos;
        self.next_boundary_from(l, pos);
    }

    /// Skip to next grapheme.
    fn advance(&mut self, l: &mut Lexer) {
        l.start = l.end.cur_cursor();
        self.next_boundary(l);
    }

    fn is(&self, l: &Lexer, character: u8) -> bool {
        if l.has_chars() {
            self.file(l).src.as_bytes()[l.start] == character
        } else {
            false
        }
    }

    fn is_str(&self, l: &Lexer, slice: &[u8]) -> bool {
        if slice.len() > self.file(l).src.len() - l.start {
            return false;
        }
        for (a, b) in self.file(l).src.as_bytes()[l.start..].iter().zip(slice.iter()) {
            if a != b { return false; }
        }
        true
    }

    /// Skip over `n` bytes of known-ASCII text
    unsafe fn advance_by_ascii(&mut self, l: &mut Lexer, n: usize) {
        l.start = std::cmp::min(l.start + n, self.file(l).src.len());
        self.next_boundary_from(l, l.end.cur_cursor() + n - 1);
    }

    fn is_letter(&self, l: &Lexer) -> bool {
        let mut chars = self.cur_grapheme(l).chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_alphabetic()
        } else {
            false
        }
    }
    fn is_hex_digit(&self, l: &Lexer) -> bool {
        let mut chars = self.cur_grapheme(l).chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_ascii_hexdigit()
        } else {
            false
        }
    }
    fn is_newline(&self, l: &Lexer) -> bool { self.is(l, b'\n') || self.is(l, b'\r') || self.is_str(l, b"\r\n") }
    fn is_whitespace(&self, l: &Lexer) -> bool { self.is(l, b' ') || self.is(l, b'\t') }
    fn is_num(&self, l: &Lexer) -> bool {
        let mut chars = self.cur_grapheme(l).chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_numeric()
        } else {
            false
        }
    }

    fn pack_tok(&self, l: &mut Lexer, kind: TokenKind) -> (TokenKind, SourceRange) {
        let range = l.tok_start_loc..l.cur_loc();
        let range = l.make_src_range(range);
        l.tok_start_loc = l.cur_loc();
        (kind, range)
    }

    fn l_next(&mut self, l: &mut Lexer) -> Result<(TokenKind, SourceRange), ()> {
        if !l.has_chars() {
            Ok(self.pack_tok(l, TokenKind::Eof))
        } else if self.is_newline(l) {
            while l.has_chars() && self.is_newline(l) {
                self.advance(l);
            }
            Ok(self.pack_tok(l, TokenKind::Newline))
        } else if self.is_whitespace(l) {
            while l.has_chars() && self.is_whitespace(l) {
                self.advance(l);
            }
            Ok(self.pack_tok(l, TokenKind::Whitespace))
        } else if self.is_str(l, b"//") {
            unsafe { self.advance_by_ascii(l, 2); }
            while l.has_chars() && !self.is_newline(l) {
                self.advance(l);
            }
            Ok(self.pack_tok(l, TokenKind::SingleLineComment))
        } else if self.is_str(l, b"/*") {
            let mut comment_begin = l.cur_loc();
            let mut levels = 1;
            unsafe { self.advance_by_ascii(l, 2); }
            let mut prev_ending_delimiter = None;
            while l.has_chars() {
                if self.is_str(l, b"/*") {
                    levels += 1;
                    comment_begin = l.cur_loc();
                    self.advance(l);
                } else if self.is_str(l, b"*/") {
                    prev_ending_delimiter = Some(l.cur_loc());
                    levels -= 1;
                    self.advance(l);
                    assert!(levels >= 0);
                    if levels == 0 {
                        self.advance(l);
                        break;
                    }
                }
                self.advance(l);
            }
            if levels > 0 {
                let range = l.make_src_range(comment_begin..(comment_begin+2));
                let mut err = Error::new(
                    "unterminated '/*' comment"
                ).adding_primary_range(
                    range,
                    "previous '/*' delimiter here"
                );
                if let Some(prev_ending_delimiter) = prev_ending_delimiter {
                    let range = l.make_src_range(
                        prev_ending_delimiter..(prev_ending_delimiter + 2)
                    );
                    err.add_primary_range(range, "previous '*/' delimiter here");
                    // Reset the position to the position right after the previous ending delimiter so we can keep lexing.
                    // This might be a terrible idea, we'll just have to wait and see.
                    self.set_pos(l, prev_ending_delimiter + 2);
                }
                self.diag.push(err);
            }
            Ok(self.pack_tok(l, TokenKind::MultiLineComment))
        } else if self.is_str(l, b"*/") {
            unsafe { self.advance_by_ascii(l, 2); }
            let range = l.make_src_range(l.tok_start_loc..l.cur_loc());
            self.diag.push(
                Error::new(
                    "unexpected '*/' delimiter"
                ).adding_primary_range(
                    range,
                    "no previous '/*' to match"
                )
            );
            self.l_next(l)
        } else if self.is(l, b'"') {
            self.advance(l);
            let mut in_escape_mode = false;
            let mut lit = String::new();
            let mut terminated = false;
            while l.has_chars() && !self.is_newline(l) {
                let char_to_insert = if in_escape_mode {
                    if let Some(character) = l.special_escape_characters.get(self.cur_grapheme(l)) {
                        character
                    } else {
                        let range = l.make_src_range(l.cur_loc()..(l.cur_loc() + 1));
                        self.diag.push(
                            Error::new(
                                format!("invalid escape character '{}'", self.cur_grapheme(l))
                            ).adding_primary_range(
                                range,
                                "escaped here"
                            )
                        );
                        self.cur_grapheme(l)
                    }
                } else {
                    self.cur_grapheme(l)
                };

                match char_to_insert {
                    "\\" => {
                        self.advance(l);
                        in_escape_mode = true;
                    },
                    "\"" if !in_escape_mode => {
                        self.advance(l);
                        terminated = true;
                        break;
                    },
                    char_to_insert => {
                        lit += char_to_insert;
                        self.advance(l);
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
                let range = l.make_src_range(l.tok_start_loc..l.tok_start_loc + 1);
                self.diag.push(
                    Error::new(
                        msg
                    ).adding_primary_range(
                        range,
                        "literal begins here"
                    )
                );
            }
            if lit.len() == 1 {
                Ok(self.pack_tok(l, TokenKind::CharLit(lit.as_bytes()[0] as i8)))
            } else {
                Ok(self.pack_tok(l, TokenKind::StrLit(CString::new(lit).unwrap())))
            }
        } else if l.has_chars() && (self.is_letter(l) || self.is(l, b'_')) {
            let ident_start = l.cur_loc();
            let ident_end;
            loop {
                self.advance(l);

                if !l.has_chars() || (!self.is_letter(l) && !self.is(l, b'_') && !self.is_num(l)) {
                    ident_end = l.cur_loc();
                    break;
                }
            }

            let ident_bytes = &self.src_map.files[l.file].src.as_bytes()[ident_start..ident_end];
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
                "extern_mod" => ExternModule,
                "_debug_mark" => DebugMark,
                _ => {
                    let ident = self.interner.get_or_intern(ident);
                    Ident(ident)
                },
            };
            Ok(self.pack_tok(l, kind))
        } else if l.has_chars() && self.is_num(l) {
            let mut has_dot = false;
            let mut last_was_dot = None;
            loop {
                if !l.has_chars() || (!self.is_num(l) && !self.is(l, b'.')) {
                    break;
                }
                if self.is(l, b'.') {
                    if has_dot {
                        break;
                    } else {
                        has_dot = true;
                    }
                    last_was_dot = Some(l.cur_loc());
                } else {
                    last_was_dot = None;
                }
                self.advance(l);
            }

            if let Some(dot_pos) = last_was_dot {
                self.set_pos(l, dot_pos);
                has_dot = false;
            }
            let lit = &self.file(l).src[l.tok_start_loc..l.cur_loc()];
            if has_dot {
                Ok(self.pack_tok(l, TokenKind::DecLit(lit.parse().unwrap())))
            } else {
                Ok(self.pack_tok(l, TokenKind::IntLit(lit.parse().unwrap())))
            }
        } else if self.is(l, b'$') {
            l.tok_start_loc = l.cur_loc();

            // Ignore the dollar sign
            self.advance(l);

            // Don't include the dollar sign in the number that we parse
            let lit_start_loc = l.cur_loc();

            while l.has_chars() && self.is_hex_digit(l) {
                self.advance(l);
            }
            let lit = &self.file(l).src[lit_start_loc..l.cur_loc()];
            Ok(self.pack_tok(l, TokenKind::IntLit(u64::from_str_radix(lit, 16).unwrap())))
        } else {
            macro_rules! match_symbols {
                ($($kind: ident $symbol: expr)+) => {
                    if false { unreachable!() }
                    $(
                        else if self.is_str(l, $symbol) {
                            // symbols are all ASCII, so it's safe to assume that number of grapheme clusters == number of bytes
                            unsafe { self.advance_by_ascii(l, $symbol.len()); }
                            Ok(self.pack_tok(l, TokenKind::$kind))
                        }
                    )+
                    else {
                        let range = l.make_src_range(l.tok_start_loc..(l.cur_loc() + 1));
                        self.diag.push(
                            Error::new(
                                format!("unrecognized token '{}'", self.cur_grapheme(l))
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
