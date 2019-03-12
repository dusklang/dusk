use std::collections::HashMap;
use unicode_segmentation::UnicodeSegmentation;

use crate::source_info::SourceFile;
use crate::token::{Token, TokenKind};

pub struct Lexer<'src> {
    file: &'src mut SourceFile<'src>,
    pos: usize,
    cur_tok_start: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(file: &'src mut SourceFile<'src>) -> Lexer<'src> {
        Lexer {
            file,
            pos: 0,
            cur_tok_start: 0,
        }
    }

    fn cur_char(&self) -> &str { self.file.src[self.pos] }
    fn is(&self, character: char) -> bool { 
        if self.pos >= self.file.src.len() {
            false
        } else {
            self.cur_char() == character.to_string()
        }
    }
    fn is_str(&self, slice: &str) -> bool {
        if self.pos >= self.file.src.len() {
            false
        } else {
            let graphemes = UnicodeSegmentation::graphemes(slice, true).collect::<Vec<_>>();
            &graphemes[..] == &self.file.src[self.pos..]
        }
    }
    fn is_letter(&self) -> bool {
        let mut chars = self.cur_char().chars();
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
        let mut chars = self.cur_char().chars();
        if let Some(character) = chars.next() {
            if let Some(_) = chars.next() { false }
            else {
                character.is_numeric()
            }
        } else {
            false
        }
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

        while(self.pos < self.file.src.len()) {
            if self.is('\n') {
                self.file.next_line_position(self.pos + 1);
            } else if self.is_str("\r\n") {
                self.pos += 1;
                self.file.next_line_position(self.pos + 1);
            } else if self.is('\r') {
                self.file.next_line_position(self.pos + 1);
            }
            self.pos += 1;
        }
        self.

        result
    }
}