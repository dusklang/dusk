use std::collections::HashMap;
use unicode_segmentation::{UnicodeSegmentation, GraphemeIndices};

use crate::token::{Token, TokenKind};

pub struct Lexer<'src> {
    src: &'src str,
    gr: Vec<(usize, &'src str)>,
    pos: usize,
    tok_begin_pos: usize,
    lines: &'src mut Vec<u32>,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str, lines: &'src mut Vec<u32>) -> Lexer<'src> {
        Lexer {
            src,
            gr: UnicodeSegmentation::grapheme_indices(src, true).collect(),
            pos: 0,
            tok_begin_pos: 0,
            lines,
        }
    }

    #[inline]
    fn cur(&self) -> &str {
        self.gr[self.pos].1
    }

    #[inline]
    fn has_chars(&self) -> bool {
        self.pos < self.gr.len()
    }

    #[inline]
    fn is(&self, character: char) -> bool {
        if self.has_chars() {
            self.cur() == character.encode_utf8(&mut [0; 4])
        } else {
            false
        }
    }
    fn is_str(&self, slice: &str) -> bool {
        if self.has_chars() {
            let mut slice = UnicodeSegmentation::graphemes(slice, true);
            let mut src = self.gr[self.pos..].iter().map(|(_, c)| c);
            loop {
                match (slice.next(), src.next()) {
                    (Some(c1), Some(c2)) => if c1 != *c2 { return false },
                    (Some(_), None) | (None, Some(_)) => return false,
                    (None, None) => return true
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
    fn is_newline(&self) -> bool { self.is('\n') || self.is('\r') }
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

    #[inline]
    fn make_tok(&self, kind: TokenKind<'src>) -> Token<'src> {
        Token::new(kind, (self.tok_begin_pos as u32)..(self.pos as u32))
    }

    // TODO: Lex one token at a time.
    pub fn lex(&'src mut self) -> Vec<Token<'src>> {
        let special_escape_characters = {
            let mut map = HashMap::new();
            map.insert('n', '\n');
            map.insert('"', '"');
            map.insert('0', '\0');
            map.insert('\\', '\\');
            map
        };

        let mut result = Vec::new();

        // This was in the old lexer, but I don't know why?
        while self.has_chars() {
            if self.is('\n') {
                self.lines.push(self.pos as u32 + 1);
            } else if self.is_str("\r\n") {
                self.pos += 1;
                self.lines.push(self.pos as u32 + 1);
            } else if self.is('\r') {
                self.lines.push(self.pos as u32 + 1);
            }
            self.pos += 1;
        }
        self.pos = 0;

        loop {
            // EOF.
            if self.pos == self.gr.len() { 
                result.push(self.make_tok(TokenKind::Eof));
                break;
            }

            // Newlines.
            let mut saw_newline = false;
            while self.has_chars() && self.is_newline() { 
                saw_newline = true;
                self.pos += 1; 
            }
            if saw_newline { result.push(self.make_tok(TokenKind::Newline)); }
        }

        result
    }
}