use std::collections::HashMap;
use unicode_segmentation::GraphemeCursor;
use string_interner::DefaultStringInterner;

use crate::token::{TokenVec, TokenKind};
use crate::error::Error;

#[inline]
pub fn lex<'src>(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec, DefaultStringInterner, Vec<Error>) {
    Lexer::lex(src, lines)
}

struct Lexer<'src> {
    src: &'src str,
    /// Start of current grapheme
    start: usize,
    /// End of current grapheme
    end: GraphemeCursor,

    tok_start_loc: usize,
    lines: &'src mut Vec<usize>,
    interner: DefaultStringInterner,
    toks: TokenVec,
}

impl<'src> Lexer<'src> {
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

    /// Pushes a new token with given kind and source range of `tok_start_loc`..`cur_loc()`
    /// to token tree, then sets `tok_start_loc` to `cur_loc()`.
    fn push(&mut self, kind: TokenKind) {
        let range = self.tok_start_loc..self.cur_loc();
        self.tok_start_loc = self.cur_loc();
        self.toks.push(kind, range);
    }

    fn lex(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec, DefaultStringInterner, Vec<Error>) {
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
            lines,
            interner: DefaultStringInterner::default(),
            toks: TokenVec::new(),
        };
        l.next_boundary();

        let mut errs = Vec::new();

        // Find line breaks.
        while l.has_chars() {
            if l.is(b'\n') {
                l.advance();
                l.lines.push(l.cur_loc());
            } else if l.is_str(b"\r\n") {
                unsafe { l.advance_by_ascii(2); }
                l.lines.push(l.cur_loc());
            } else if l.is(b'\r') {
                l.advance();
                l.lines.push(l.cur_loc());
            } else {
                l.advance();
            }
        }
        l.set_pos(0);

        // TODO: This loop would likely be an excellent candidate for conversion to a finite
        // state machine. However, implementing it in the obvious Rusty way (a state variable and a
        // loop that matches over and mutates it) would unnecessarily pessimize performance
        // because--save for heroic optimizations that rustc doesn't do--when we enter a state,
        // we wouldn't be able to enter a tight loop. Instead we'd have to jump to the beginning
        // of the match statement every time, thrashing the instruction cache and performing unnecessary
        // checks.
        loop {
            // EOF.
            if !l.has_chars() {
                l.push(TokenKind::Eof);
                break;
            }

            // Newlines.
            let mut found_tok = false;
            while l.has_chars() && l.is_newline() { 
                found_tok = true;
                l.advance(); 
            }
            if found_tok { l.push(TokenKind::Newline); }

            // Whitespace.
            found_tok = false;
            while l.has_chars() && l.is_whitespace() {
                found_tok = true;
                l.advance();
            }
            if found_tok { l.push(TokenKind::Whitespace); }

            // Single-line comments.
            if l.is_str(b"//") {
                unsafe { l.advance_by_ascii(2); }
                while l.has_chars() && !l.is_newline() {
                    l.advance();
                }
                l.push(TokenKind::SingleLineComment);
            }

            // Multi-line comments.
            if l.is_str(b"/*") {
                let mut comment_begin = l.cur_loc();
                let mut levels = 1;
                unsafe { l.advance_by_ascii(2); }
                let mut prev_ending_delimiter = None;
                while l.has_chars() {
                    if l.is_str(b"/*") {
                        levels += 1;
                        comment_begin = l.cur_loc();
                        l.advance();
                    } else if l.is_str(b"*/") {
                        prev_ending_delimiter = Some(l.cur_loc());
                        levels -= 1;
                        l.advance();
                        assert!(levels >= 0);
                        if levels == 0 {
                            l.advance();
                            break;
                        }
                    }
                    l.advance();
                }
                if levels > 0 {
                    let mut err = Error::new(
                        "unterminated '/*' comment"
                    ).adding_primary_range(
                        comment_begin..l.cur_loc(),
                        "previous '/*' delimiter here"
                    );
                    if let Some(prev_ending_delimiter) = prev_ending_delimiter {
                        err.add_primary_range(prev_ending_delimiter..(prev_ending_delimiter + 2), "previous '*/' delimiter here");
                        // Reset the position to the position right after the previous ending delimiter so we can keep lexing.
                        // This might be a terrible idea, we'll just have to wait and see.
                        l.set_pos(prev_ending_delimiter + 2);
                    }
                    errs.push(err);
                }
                l.push(TokenKind::MultiLineComment);
            }
            if l.is_str(b"*/") {
                unsafe { l.advance_by_ascii(2); }
                errs.push(
                    Error::new(
                        "unexpected '*/' delimiter"
                    ).adding_primary_range(
                        l.tok_start_loc..l.cur_loc(),
                        "no previous '/*' to match"
                    )
                );
            }

            macro_rules! match_tokens {
                ($($kind: ident $symbol: expr)+) => {
                    if false {}
                    $(
                        else if l.is_str($symbol) {
                            // symbols are all ASCII, so it's safe to assume that number of grapheme clusters == number of bytes
                            unsafe { l.advance_by_ascii($symbol.len()); }
                            l.push(TokenKind::$kind);
                        }
                    )+
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
            );

            // String and character literals.
            if l.is(b'"') {
                l.advance();
                let mut in_escape_mode = false;
                let mut lit = String::new();
                let mut terminated = false;
                while l.has_chars() && !l.is_newline() {
                    let char_to_insert = if in_escape_mode {
                        in_escape_mode = false;
                        if let Some(character) = special_escape_characters.get(l.cur()) {
                            character
                        } else {
                            errs.push(
                                Error::new(
                                    format!("invalid escape character '{}'", l.cur())
                                ).adding_primary_range(
                                    l.cur_loc()..(l.cur_loc() + 1), 
                                    "escaped here"
                                )
                            );
                            l.cur()
                        }
                    } else {
                        // This is same expression that l.cur() returns, but calling that method makes
                        // the borrow checker complain. :(
                        l.cur()
                    };

                    match char_to_insert {
                        "\\" => {
                            l.advance();
                            in_escape_mode = true;
                        },
                        "\"" => {
                            l.advance();
                            terminated = true;
                            break;
                        },
                        char_to_insert => {
                            lit += char_to_insert;
                            l.advance();
                        }
                    }
                }
                if !terminated {
                    let msg = if lit.len() == 1 {
                        "unterminated character literal"
                    } else {
                        "unterminated string literal"
                    };
                    errs.push(
                        Error::new(
                            msg
                        ).adding_primary_range(
                            l.tok_start_loc..l.tok_start_loc + 1,
                            "literal begins here"
                        )
                    );
                }
                if lit.len() == 1 {
                    l.push(TokenKind::CharLit(lit.as_bytes()[0]));
                } else {
                    l.push(TokenKind::StrLit(lit));
                }
            }
            // Identifiers and keywords.
            if l.has_chars() && (l.is_letter() || l.is(b'_')) {
                let ident_start = l.cur_loc();
                let ident_end;
                loop {
                    l.advance();

                    if !l.has_chars() || (!l.is_letter() && !l.is(b'_') && !l.is_num()) {
                        ident_end = l.cur_loc();
                        break;
                    }
                }

                let ident_bytes = &l.src.as_bytes()[ident_start..ident_end];
                let ident = unsafe { std::str::from_utf8_unchecked(ident_bytes) };

                use TokenKind::*;
                let kind = match ident {
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
                    _ => Ident(l.interner.get_or_intern(ident)),
                };
                l.push(kind);
            }
            // Integer and decimal literals.
            if l.has_chars() && l.is_num() {
                let mut has_dot = false;
                loop {
                    // TODO: When the char after the '.' is not a number,
                    // we should output the '.' as its own token so something
                    // like 5.member would work. Not a priority right now though,
                    // obviously.
                    if l.is(b'.') {
                        if has_dot {
                            break;
                        } else {
                            has_dot = true;
                        }
                    }
                    if !l.has_chars() || (!l.is_num() && !l.is(b'.')) {
                        break;
                    }
                    l.advance();
                }

                let lit = &l.src[l.tok_start_loc..l.cur_loc()];
                if has_dot {
                    l.push(TokenKind::DecLit(lit.parse().unwrap()));
                } else {
                    l.push(TokenKind::IntLit(lit.parse().unwrap()));
                }
            }
        }

        (l.toks, l.interner, errs)
    }
}
