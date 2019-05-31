use std::collections::HashMap;
use unicode_segmentation::GraphemeCursor;

use crate::token::{TokenVec, TokenKind};
use crate::error::Error;

#[inline]
pub fn lex<'src>(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec, Vec<Error>) {
    Lexer::lex(src, lines)
}

struct Lexer<'src> {
    gr: GraphemeState<'src>,
    tok_start_loc: usize,
    lines: &'src mut Vec<usize>,
    toks: TokenVec,
}

struct GraphemeState<'src> {
    src: &'src str,
    /// Start of current grapheme
    start: usize,
    /// End of current grapheme
    end: GraphemeCursor,
}

struct SavedGraphemeState {
    start: usize,
    end: usize,
}

impl<'src> GraphemeState<'src> {
    fn new(src: &'src str) -> Self {
        let mut new = GraphemeState {
            src,
            start: 0,
            end:   GraphemeCursor::new(0, src.len(), true),
        };
        new.end.next_boundary(src, 0).unwrap();

        new
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

    fn set_pos(&mut self, pos: usize) {
        self.start = pos;
        self.end.set_cursor(pos);
        self.end.next_boundary(self.src, 0).unwrap();
    }

    /// Skip to next grapheme.
    fn advance(&mut self) {
        self.start = self.end.cur_cursor();
        self.end.next_boundary(self.src, 0).unwrap();
    }

    /// Skip over `n` bytes of ASCII
    unsafe fn advance_by_ascii(&mut self, n: usize) {
        self.start = std::cmp::min(self.start + n, self.src.len());
        self.end.set_cursor(std::cmp::min(self.end.cur_cursor() + n - 1, self.src.len()));
        self.end.next_boundary(self.src, 0).unwrap();
    }

    fn save_state(&self) -> SavedGraphemeState {
        SavedGraphemeState {
            start: self.start,
            end: self.end.cur_cursor(),
        }
    }

    fn restore_state(&mut self, state: SavedGraphemeState) {
        self.start = state.start;
        self.end.set_cursor(state.end);
    }
}

impl<'src> Lexer<'src> {
    fn is(&self, character: char) -> bool {
        if self.gr.has_chars() {
            self.gr.cur() == character.encode_utf8(&mut [0; 4])
        } else {
            false
        }
    }

    // TODO: this method only requires a mutable ref to self because it uses the unicode helper methods above
    //       I should instead factor them out into a separate struct so I can reuse them here
    #[inline(never)]
    fn is_str(&mut self, slice: &str) -> bool {
        debug_assert!(slice.is_ascii() && !slice.is_empty());
        if self.gr.has_chars() {
            let saved = self.gr.save_state();
            let mut slice = slice.bytes();
            let mut equal = self.gr.has_chars();
            while self.gr.has_chars() {
                if let Some(c1) = slice.next() {
                    let c2 = self.gr.cur();
                    // Safe if slice is ASCII, as is asserted above
                    if unsafe { std::str::from_utf8_unchecked(&[c1]) } != c2 {
                        equal = false;
                        break;
                    }
                } else {
                    break;
                }
                self.gr.advance();
            }
            self.gr.restore_state(saved);
            equal
        } else {
            false
        }
    }
    fn is_letter(&self) -> bool {
        let mut chars = self.gr.cur().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_alphabetic()
        } else {
            false
        }
    }
    fn is_newline(&mut self) -> bool { self.is('\n') || self.is('\r') || self.is_str("\r\n") }
    fn is_whitespace(&self) -> bool { self.is(' ') || self.is('\t') }
    fn is_num(&self) -> bool {
        let mut chars = self.gr.cur().chars();
        if let Some(character) = chars.next() {
            chars.next().is_none() && character.is_numeric()
        } else {
            false
        }
    }

    /// Pushes a new token with given kind and source range of `tok_start_loc`..`cur_loc()`
    /// to token tree, then sets `tok_start_loc` to `cur_loc()`.
    fn push(&mut self, kind: TokenKind) {
        let range = self.tok_start_loc..self.gr.cur_loc();
        self.tok_start_loc = self.gr.cur_loc();
        self.toks.push(kind, range);
    }

    fn lex(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec, Vec<Error>) {
        let special_escape_characters = {
            let mut map = HashMap::new();
            map.insert("n", "\n");
            map.insert("\"", "\"");
            map.insert("0", "\0");
            map.insert("\\", "\\");
            map
        };

        let mut l = Lexer {
            gr: GraphemeState::new(src),
            tok_start_loc: 0,
            lines,
            toks: TokenVec::new(),
        };

        let mut errs = Vec::new();

        // Find line breaks.
        while l.gr.has_chars() {
            if l.is('\n') {
                l.gr.advance();
                l.lines.push(l.gr.cur_loc());
            } else if l.is_str("\r\n") {
                unsafe { l.gr.advance_by_ascii(2); }
                l.lines.push(l.gr.cur_loc());
            } else if l.is('\r') {
                l.gr.advance();
                l.lines.push(l.gr.cur_loc());
            } else {
                l.gr.advance();
            }
        }
        l.gr.set_pos(0);

        // TODO: This loop would likely be an excellent candidate for conversion to a finite
        // state machine. However, implementing it in the obvious Rusty way (a state variable and a
        // loop that matches over and mutates it) would unnecessarily pessimize performance
        // because--save for heroic optimizations that rustc doesn't do--when we enter a state,
        // we wouldn't be able to enter a tight loop. Instead we'd have to jump to the beginning
        // of the match statement every time, thrashing the instruction cache and performing unnecessary
        // checks.
        loop {
            // EOF.
            if !l.gr.has_chars() {
                l.push(TokenKind::Eof);
                break;
            }

            // Newlines.
            let mut found_tok = false;
            while l.gr.has_chars() && l.is_newline() { 
                found_tok = true;
                l.gr.advance(); 
            }
            if found_tok { l.push(TokenKind::Newline); }

            // Whitespace.
            found_tok = false;
            while l.gr.has_chars() && l.is_whitespace() {
                found_tok = true;
                l.gr.advance();
            }
            if found_tok { l.push(TokenKind::Whitespace); }

            // Single-line comments.
            if l.is_str("//") {
                unsafe { l.gr.advance_by_ascii(2); }
                while l.gr.has_chars() && !l.is_newline() {
                    l.gr.advance();
                }
                l.push(TokenKind::SingleLineComment);
            }

            // Multi-line comments.
            if l.is_str("/*") {
                let mut comment_begin = l.gr.cur_loc();
                let mut levels = 1;
                unsafe { l.gr.advance_by_ascii(2); }
                let mut prev_ending_delimiter = None;
                while l.gr.has_chars() {
                    if l.is_str("/*") {
                        levels += 1;
                        comment_begin = l.gr.cur_loc();
                        l.gr.advance();
                    } else if l.is_str("*/") {
                        prev_ending_delimiter = Some(l.gr.cur_loc());
                        levels -= 1;
                        l.gr.advance();
                        assert!(levels >= 0);
                        if levels == 0 {
                            l.gr.advance();
                            break;
                        }
                    }
                    l.gr.advance();
                }
                if levels > 0 {
                    let mut err = Error::new(
                        "unterminated '/*' comment"
                    ).adding_primary_range(
                        comment_begin..l.gr.cur_loc(),
                        "previous '/*' delimiter here"
                    );
                    if let Some(prev_ending_delimiter) = prev_ending_delimiter {
                        err.add_primary_range(prev_ending_delimiter..(prev_ending_delimiter + 2), "previous '*/' delimiter here");
                        // Reset the position to the position right after the previous ending delimiter so we can keep lexing.
                        // This might be a terrible idea, we'll just have to wait and see.
                        l.gr.set_pos(prev_ending_delimiter + 2);
                    }
                    errs.push(err);
                }
                l.push(TokenKind::MultiLineComment);
            }
            if l.is_str("*/") {
                unsafe { l.gr.advance_by_ascii(2); }
                errs.push(
                    Error::new(
                        "unexpected '*/' delimiter"
                    ).adding_primary_range(
                        l.tok_start_loc..l.gr.cur_loc(),
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
                            unsafe { l.gr.advance_by_ascii($symbol.len()); }
                            l.push(TokenKind::$kind);
                        }
                    )+
                }
            }
            match_tokens!(
                Colon               ":"
                Comma               ","
                LeftParen           "("
                RightParen          ")"
                Dot                 "."
                OpenCurly           "{"
                CloseCurly          "}"

                AddAssign           "+="
                SubAssign           "-="
                MultAssign          "*="
                DivAssign           "/="
                ModAssign           "%="
                BitwiseOrAssign     "|="
                BitwiseAndAssign    "&="
                Add                 "+"
                Sub                 "-"
                Asterisk            "*"
                Div                 "/"
                Mod                 "%"
                Equal               "=="
                NotEqual            "!="
                LTE                 "<="
                LT                  "<"
                GTE                 ">="
                GT                  ">"
                LogicalOr           "||"
                LogicalAnd          "&&"
                LogicalNot          "!"
                Assign              "="
                Ampersand           "&"
                Pipe                "|"
            );

            // String and character literals.
            if l.is('"') {
                l.gr.advance();
                let mut in_escape_mode = false;
                let mut lit = String::new();
                let mut terminated = false;
                while l.gr.has_chars() && !l.is_newline() {
                    let char_to_insert = if in_escape_mode {
                        in_escape_mode = false;
                        if let Some(character) = special_escape_characters.get(l.gr.cur()) {
                            character
                        } else {
                            errs.push(
                                Error::new(
                                    format!("invalid escape character '{}'", l.gr.cur())
                                ).adding_primary_range(
                                    l.gr.cur_loc()..(l.gr.cur_loc() + 1), 
                                    "escaped here"
                                )
                            );
                            l.gr.cur()
                        }
                    } else {
                        // This is same expression that l.gr.cur() returns, but calling that method makes
                        // the borrow checker complain. :(
                        l.gr.cur()
                    };

                    match char_to_insert {
                        "\\" => {
                            l.gr.advance();
                            in_escape_mode = true;
                        },
                        "\"" => {
                            l.gr.advance();
                            terminated = true;
                            break;
                        },
                        char_to_insert => {
                            lit += char_to_insert;
                            l.gr.advance();
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
            let mut text = String::new();
            if l.gr.has_chars() && (l.is_letter() || l.is('_')) {
                loop {
                    text += l.gr.cur();
                    l.gr.advance();

                    if !l.gr.has_chars() || (!l.is_letter() && !l.is('_') && !l.is_num()) {
                        break;
                    }
                }

                use TokenKind::*;
                let kind = match &*text {
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
                    _ => Ident(text),
                };
                l.push(kind);
            }
            // Integer and decimal literals.
            if l.gr.has_chars() && l.is_num() {
                let mut has_dot = false;
                loop {
                    // TODO: When the char after the '.' is not a number,
                    // we should output the '.' as its own token so something
                    // like 5.member would work. Not a priority right now though,
                    // obviously.
                    if l.is('.') {
                        if has_dot {
                            break;
                        } else {
                            has_dot = true;
                        }
                    }
                    if !l.gr.has_chars() || (!l.is_num() && !l.is('.')) {
                        break;
                    }
                    l.gr.advance();
                }

                let lit = &l.gr.src[l.tok_start_loc..l.gr.cur_loc()];
                if has_dot {
                    l.push(TokenKind::DecLit(lit.parse().unwrap()));
                } else {
                    l.push(TokenKind::IntLit(lit.parse().unwrap()));
                }
            }
        }

        (l.toks, errs)
    }
}
