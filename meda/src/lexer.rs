use std::collections::HashMap;
use std::ops::Range;
use unicode_segmentation::{UnicodeSegmentation, GraphemeIndices};

use crate::token::{Token, TokenKind};
use crate::source_info::SourceRange;
use crate::error::Error;

pub struct Lexer<'src> {
    src: &'src str,
    gr: Vec<(usize, &'src str)>,
    pos: usize,
    tok_start_pos: usize,
    lines: &'src mut Vec<usize>,
    tokens: Vec<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str, lines: &'src mut Vec<usize>) -> Lexer<'src> {
        Lexer {
            src,
            gr: UnicodeSegmentation::grapheme_indices(src, true).collect(),
            pos: 0,
            tok_start_pos: 0,
            lines,
            tokens: Vec::new(),
        }
    }

    fn cur(&self) -> &str {
        self.gr[self.pos].1
    }

    fn has_chars(&self) -> bool {
        self.pos < self.gr.len()
    }

    fn is(&self, character: char) -> bool {
        if self.has_chars() {
            self.cur() == character.encode_utf8(&mut [0; 4])
        } else {
            false
        }
    }

    /// NOTE: Assumes `slice` is ASCII!
    fn is_str(&self, slice: &str) -> bool {
        if self.has_chars() {
            let mut slice = slice.bytes();
            let mut src = self.gr[self.pos..].iter().map(|(_, c)| c);
            loop {
                match (slice.next(), src.next()) {
                    (Some(c1), Some(c2)) => {
                        if std::str::from_utf8(&[c1]).unwrap() != *c2 {
                            return false;
                        }
                    },
                    (Some(_), None) => return false,
                    (None, Some(_)) | (None, None) => return true
                }
            }
        } else {
            false
        }
    }
    fn is_letter(&self) -> bool {
        let mut chars = self.cur().chars();
        if let Some(character) = chars.next() {
            if let Some(_) = chars.next() { false }
            else {
                character.is_alphabetic()
            }
        } else {
            false
        }
    }
    fn is_newline(&self) -> bool { self.is('\n') || self.is('\r') || self.is_str("\r\n") }
    fn is_whitespace(&self) -> bool { self.is(' ') || self.is('\t') }
    fn is_num(&self) -> bool {
        let mut chars = self.cur().chars();
        if let Some(character) = chars.next() {
            if let Some(_) = chars.next() { false }
            else {
                character.is_numeric()
            }
        } else {
            false
        }
    }

    fn gr_index_to_src_index(&self, index: usize) -> usize {
        if index < self.gr.len() {
            self.gr[index].0
        } else {
            self.src.len()
        }
    }
    fn gr_range_to_src_range(&self, range: Range<usize>) -> SourceRange {
        self.gr_index_to_src_index(range.start)
            ..self.gr_index_to_src_index(range.end)
    }

    /// Pushes a new token with given kind and source range of `tok_start_pos`..`pos`
    /// to `tokens`, then sets `tok_start_pos` to `pos`.
    fn push(&mut self, kind: TokenKind) {
        let range = self.gr_range_to_src_range(self.tok_start_pos..self.pos);
        self.tok_start_pos = self.pos;
        self.tokens.push(Token::new(kind, range))
    }

    pub fn lex(mut self) -> Result<Vec<Token>, Error> {
        let special_escape_characters = {
            let mut map = HashMap::new();
            map.insert("n", "\n");
            map.insert("\"", "\"");
            map.insert("0", "\0");
            map.insert("\\", "\\");
            map
        };

        // This was in the old lexer, but I don't know why?
        while self.has_chars() {
            if self.is('\n') {
                self.lines.push(self.pos + 1);
            } else if self.is_str("\r\n") {
                self.pos += 1;
                self.lines.push(self.pos + 1);
            } else if self.is('\r') {
                self.lines.push(self.pos + 1);
            }
            self.pos += 1;
        }
        self.pos = 0;

        loop {
            // EOF.
            if self.pos == self.gr.len() { 
                self.push(TokenKind::Eof);
                break;
            }

            // Newlines.
            let mut found_tok = false;
            while self.has_chars() && self.is_newline() { 
                found_tok = true;
                self.pos += 1; 
            }
            if found_tok { self.push(TokenKind::Newline); }

            // Whitespace.
            found_tok = false;
            while self.has_chars() && self.is_whitespace() {
                found_tok = true;
                self.pos += 1;
            }
            if found_tok { self.push(TokenKind::Whitespace); }

            // Single-line comments.
            if self.is_str("//") {
                self.pos += 2;
                while self.has_chars() && !self.is_newline() {
                    self.pos += 1;
                }
                self.push(TokenKind::SingleLineComment);
            }

            // Multi-line comments.
            if self.is_str("/*") {
                let mut comment_begin = self.pos;
                let mut levels = 1;
                self.pos += 2;
                while self.has_chars() {
                    if self.is_str("/*") {
                        levels += 1;
                        comment_begin = self.pos;
                        self.pos += 1;
                    } else if self.is_str("*/") {
                        levels -= 1;
                        self.pos += 1;
                        assert!(levels >= 0);
                        if levels == 0 {
                            self.pos += 1;
                            break;
                        }
                    }
                    self.pos += 1;
                }
                if levels > 0 {
                    return Err(
                        Error::new(
                            "unterminated '/*' comment",
                            self.gr_range_to_src_range(comment_begin..self.pos),
                            "last comment begins here"
                        )
                    );
                }
                self.push(TokenKind::MultiLineComment);
            }
            macro_rules! match_tokens {
                ($($kind: ident $symbol: expr)+) => {
                    if false {}
                    $(
                        else if self.is_str($symbol) {
                            if $symbol == "/" {
                                println!("range: {:?}", self.gr_range_to_src_range(self.tok_start_pos..self.pos));
                            }
                            self.pos += UnicodeSegmentation::graphemes($symbol, true).count();
                            self.push(TokenKind::$kind);
                        }
                    )+
                }
            }
            match_tokens!(
                Colon               ":"
                Comma               ","
                LeftParen           "("
                RightParen          ")"
                LeftCurly           "{"
                RightCurly          "}"
                Dot                 "."

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
            if self.is('"') {
                self.pos += 1;
                let mut in_escape_mode = false;
                let mut lit = String::new();
                let mut terminated = false;
                while self.has_chars() && !self.is_newline() {
                    let char_to_insert = if in_escape_mode {
                        if let Some(character) = special_escape_characters.get(self.cur()) {
                            character
                        } else {
                            return Err(
                                Error::new(
                                    format!("invalid escape character '{}'", self.cur()),
                                    self.pos..(self.pos + 1), 
                                    ""
                                )
                            )
                        }
                    } else {
                        // This is literally the exact same expression that self.cur() returns, but calling that method makes
                        // the borrow checker complain. :(
                        self.gr[self.pos].1
                    };

                    match char_to_insert {
                        "\\" => {
                            self.pos += 1;
                            in_escape_mode = true;
                        },
                        "\"" => {
                            self.pos += 1;
                            if lit.len() == 1 {
                                self.push(TokenKind::CharLit(lit.as_bytes()[0]));
                            } else {
                                self.push(TokenKind::StrLit(lit));
                            }
                            terminated = true;
                            break;
                        },
                        char_to_insert => {
                            lit += char_to_insert;
                            self.pos += 1;
                        }
                    }
                }
                if !terminated {
                    return Err(
                        Error::new(
                            "unterminated string literal",
                            self.gr_index_to_src_index(self.tok_start_pos)..self.gr_index_to_src_index(self.tok_start_pos + 1), 
                            "literal begins here"
                        )
                    );
                }
            }
            // Identifiers and keywords.
            let mut text = String::new();
            if self.has_chars() && (self.is_letter() || self.is('_')) {
                loop {
                    text += self.cur();
                    self.pos += 1;

                    if !self.has_chars() || (!self.is_letter() && !self.is('_') && !self.is_num()) {
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
                self.push(kind);
            }
            // Integer and decimal literals.
            if self.has_chars() && self.is_num() {
                let mut has_dot = false;
                loop {
                    // TODO: When the char after the '.' is not a number,
                    // we should output the '.' as its own token so something
                    // like 5.member would work. Not a priority right now though,
                    // obviously.
                    if self.is('.') {
                        if has_dot {
                            break;
                        } else {
                            has_dot = true;
                        }
                    }
                    if !self.has_chars() || (!self.is_num() && !self.is('.')) {
                        break;
                    }
                    self.pos += 1;
                }

                let lit = &self.src[self.gr_range_to_src_range(self.tok_start_pos..self.pos)];
                if has_dot {
                    self.push(TokenKind::DecLit(lit.parse().unwrap()));
                } else {
                    self.push(TokenKind::IntLit(lit.parse().unwrap()));
                }
            }
        }


        Ok(self.tokens)
    }
}