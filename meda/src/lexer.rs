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
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str, lines: &'src mut Vec<usize>) -> Lexer<'src> {
        Lexer {
            src,
            gr: UnicodeSegmentation::grapheme_indices(src, true).collect(),
            pos: 0,
            tok_start_pos: 0,
            lines,
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
    fn is_str(&self, slice: &str) -> bool {
        if self.has_chars() {
            let mut slice = UnicodeSegmentation::graphemes(slice, true);
            let mut src = self.gr[self.pos..].iter().map(|(_, c)| c);
            loop {
                match (slice.next(), src.next()) {
                    (Some(c1), Some(c2)) => if c1 != *c2 {
                        return false;
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

    /// Makes a new token with given kind and source range of
    /// tok_start_pos..pos, and sets tok_start_pos = pos.
    fn make_tok(&mut self, kind: TokenKind<'src>) -> Token<'src> {
        let range = self.gr_range_to_src_range(self.tok_start_pos..self.pos);
        self.tok_start_pos = self.pos;
        Token::new(kind, range)
    }

    // TODO: Lex one token at a time.
    pub fn lex(&'src mut self) -> Result<Vec<Token<'src>>, Error> {
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
                result.push(self.make_tok(TokenKind::Eof));
                break;
            }

            // Newlines.
            let mut found_tok = false;
            while self.has_chars() && self.is_newline() { 
                found_tok = true;
                self.pos += 1; 
            }
            if found_tok { result.push(self.make_tok(TokenKind::Newline)); }

            // Whitespace.
            found_tok = false;
            while self.has_chars() && self.is_whitespace() {
                found_tok = true;
                self.pos += 1;
            }
            if found_tok { result.push(self.make_tok(TokenKind::Whitespace)); }

            // Single-line comments.
            if self.is_str("//") {
                self.pos += 2;
                while self.has_chars() && !self.is_newline() {
                    self.pos += 1;
                }
                result.push(self.make_tok(TokenKind::SingleLineComment));
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
                result.push(self.make_tok(TokenKind::MultiLineComment));
            }
        }

        Ok(result)
    }
}