use std::collections::HashMap;
use unicode_segmentation::GraphemeCursor;

use crate::token::{TokenVec, TokenKind};
use crate::error::Error;
use crate::source_info::SourceRange;

#[inline(always)]
pub fn lex<'src>(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec<'src>, Vec<Error>) {
    Lexer::lex(src, lines)
}

struct Lexer<'src> {
    src: &'src str,
    /// Start of current grapheme
    start: usize,
    /// End of current grapheme
    end: GraphemeCursor,

    tok_start_loc: usize,
    errs: Vec<Error>,

    special_escape_characters: HashMap<&'static str, &'static str>,
}

impl<'src> Lexer<'src> {
    fn new(src: &'src str, lines: &'src mut Vec<usize>) -> Lexer<'src> {
        let special_escape_characters = {
            let mut map = HashMap::new();
            map.insert("n", "\n");
            map.insert("\"", "\"");
            map.insert("0", "\0");
            map.insert("\\", "\\");
            map
        };

        let mut l = Lexer {
            src,
            start: 0,
            end:   GraphemeCursor::new(0, src.len(), true),
            tok_start_loc: 0,
            special_escape_characters,
            errs: Vec::new(),
        };
        l.next_boundary();

        // Find line breaks.
        while l.has_chars() {
            if l.is(b'\n') {
                l.advance();
                lines.push(l.cur_loc());
            } else if l.is_str(b"\r\n") {
                unsafe { l.advance_by_ascii(2); }
                lines.push(l.cur_loc());
            } else if l.is(b'\r') {
                l.advance();
                lines.push(l.cur_loc());
            } else {
                l.advance();
            }
        }
        l.set_pos(0);

        l
    }

    fn cur(&self) -> &str {
        &self.src[self.start..self.end.cur_cursor()]
    }

    fn cur_loc(&self) -> usize {
        self.start
    }

    fn has_chars(&self) -> bool {
        self.start < self.end.cur_cursor()
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
        return true;
    }

    /// Skip over `n` bytes of known-ASCII text
    unsafe fn advance_by_ascii(&mut self, n: usize) {
        self.start = std::cmp::min(self.start + n, self.src.len());
        self.next_boundary_from(self.end.cur_cursor() + n - 1);
    }

    fn is_letter(&self) -> bool {
        let mut chars = self.cur().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_alphabetic()
        } else {
            false
        }
    }
    fn is_newline(&mut self) -> bool { self.is(b'\n') || self.is(b'\r') || self.is_str(b"\r\n") }
    fn is_whitespace(&self) -> bool { self.is(b' ') || self.is(b'\t') }
    fn is_num(&self) -> bool {
        let mut chars = self.cur().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_numeric()
        } else {
            false
        }
    }

    fn pack_tok(&mut self, kind: TokenKind<'src>) -> (TokenKind<'src>, SourceRange) {
        let range = self.tok_start_loc..self.cur_loc();
        self.tok_start_loc = self.cur_loc();
        (kind, range)
    }

    #[inline(never)]
    fn lex(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec<'src>, Vec<Error>) {
        let mut l = Lexer::new(src, lines);
        let mut toks = TokenVec::new();
        loop {
            let (tok, range) = l.next();
            let should_break = tok == TokenKind::Eof;
            toks.push(tok, range);
            if should_break { break; }
        }
        (toks, l.errs)
    }

    fn next(&mut self) -> (TokenKind<'src>, SourceRange) {
        if !self.has_chars() {
            self.pack_tok(TokenKind::Eof)
        } else if self.is_newline() {
            while self.has_chars() && self.is_newline() {
                self.advance();
            }
            self.pack_tok(TokenKind::Newline)
        } else if self.is_whitespace() {
            while self.has_chars() && self.is_whitespace() {
                self.advance();
            }
            self.pack_tok(TokenKind::Whitespace)
        } else if self.is_str(b"//") {
            unsafe { self.advance_by_ascii(2); }
            while self.has_chars() && !self.is_newline() {
                self.advance();
            }
            self.pack_tok(TokenKind::SingleLineComment)
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
                let mut err = Error::new(
                    "unterminated '/*' comment"
                ).adding_primary_range(
                    comment_begin..self.cur_loc(),
                    "previous '/*' delimiter here"
                );
                if let Some(prev_ending_delimiter) = prev_ending_delimiter {
                    err.add_primary_range(prev_ending_delimiter..(prev_ending_delimiter + 2), "previous '*/' delimiter here");
                    // Reset the position to the position right after the previous ending delimiter so we can keep lexing.
                    // This might be a terrible idea, we'll just have to wait and see.
                    self.set_pos(prev_ending_delimiter + 2);
                }
                self.errs.push(err);
            }
            self.pack_tok(TokenKind::MultiLineComment)
        } else if self.is_str(b"*/") {
            unsafe { self.advance_by_ascii(2); }
            self.errs.push(
                Error::new(
                    "unexpected '*/' delimiter"
                ).adding_primary_range(
                    self.tok_start_loc..self.cur_loc(),
                    "no previous '/*' to match"
                )
            );
            return self.next();
        } else if self.is(b'"') {
            self.advance();
            let mut in_escape_mode = false;
            let mut lit = String::new();
            let mut terminated = false;
            while self.has_chars() && !self.is_newline() {
                let char_to_insert = if in_escape_mode {
                    in_escape_mode = false;
                    if let Some(character) = self.special_escape_characters.get(self.cur()) {
                        character
                    } else {
                        self.errs.push(
                            Error::new(
                                format!("invalid escape character '{}'", self.cur())
                            ).adding_primary_range(
                                self.cur_loc()..(self.cur_loc() + 1), 
                                "escaped here"
                            )
                        );
                        self.cur()
                    }
                } else {
                    // This is same expression that self.cur() returns, but calling that method makes
                    // the borrow checker complain. :(
                    self.cur()
                };

                match char_to_insert {
                    "\\" => {
                        self.advance();
                        in_escape_mode = true;
                    },
                    "\"" => {
                        self.advance();
                        terminated = true;
                        break;
                    },
                    char_to_insert => {
                        lit += char_to_insert;
                        self.advance();
                    }
                }
            }
            if !terminated {
                let msg = if lit.len() == 1 {
                    "unterminated character literal"
                } else {
                    "unterminated string literal"
                };
                self.errs.push(
                    Error::new(
                        msg
                    ).adding_primary_range(
                        self.tok_start_loc..self.tok_start_loc + 1,
                        "literal begins here"
                    )
                );
            }
            if lit.len() == 1 {
                self.pack_tok(TokenKind::CharLit(lit.as_bytes()[0] as i8))
            } else {
                self.pack_tok(TokenKind::StrLit(lit))
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

            let ident_bytes: &'src [u8] = &self.src.as_bytes()[ident_start..ident_end];
            let ident: &'src str = unsafe { std::str::from_utf8_unchecked(ident_bytes) };

            use TokenKind::*;
            let kind: TokenKind<'src> = match ident {
                "fn" => Fn,
                "return" => Return,
                "true" => True,
                "false" => False,
                "if" => If,
                "else" => Else,
                "while" => While,
                "as" => As,
                "struct" => Struct,
                "do" => Do,
                "mut" => Mut,
                _ => Ident(ident),
            };
            self.pack_tok(kind)
        } else if self.has_chars() && self.is_num() {
            let mut has_dot = false;
            loop {
                // TODO: When the char after the '.' is not a number,
                // we should output the '.' as its own token so something
                // like 5.member would work. Not a priority right now though,
                // obviously.
                if self.is(b'.') {
                    if has_dot {
                        break;
                    } else {
                        has_dot = true;
                    }
                }
                if !self.has_chars() || (!self.is_num() && !self.is(b'.')) {
                    break;
                }
                self.advance();
            }

            let lit = &self.src[self.tok_start_loc..self.cur_loc()];
            if has_dot {
                self.pack_tok(TokenKind::DecLit(lit.parse().unwrap()))
            } else {
                self.pack_tok(TokenKind::IntLit(lit.parse().unwrap()))
            }
        } else {
            macro_rules! match_tokens {
                ($($kind: ident $symbol: expr)+) => {
                    if false { unreachable!() }
                    $(
                        else if self.is_str($symbol) {
                            // symbols are all ASCII, so it's safe to assume that number of grapheme clusters == number of bytes
                            unsafe { self.advance_by_ascii($symbol.len()); }
                            self.pack_tok(TokenKind::$kind)
                        }
                    )+
                    else { 
                        panic!("unrecognized token")
                    }
                }
            }
            match_tokens!(
                Colon               b":"
                Comma               b","
                LeftParen           b"("
                RightParen          b")"
                Dot                 b"."
                OpenCurly           b"{"
                CloseCurly          b"}"

                AddAssign           b"+="
                SubAssign           b"-="
                MultAssign          b"*="
                DivAssign           b"/="
                ModAssign           b"%="
                BitwiseOrAssign     b"|="
                BitwiseAndAssign    b"&="
                Add                 b"+"
                Sub                 b"-"
                Asterisk            b"*"
                Div                 b"/"
                Mod                 b"%"
                Equal               b"=="
                NotEqual            b"!="
                LTE                 b"<="
                LT                  b"<"
                GTE                 b">="
                GT                  b">"
                LogicalOr           b"||"
                LogicalAnd          b"&&"
                LogicalNot          b"!"
                Assign              b"="
                Ampersand           b"&"
                Pipe                b"|"
            )
        }
    }
}
