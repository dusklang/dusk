use std::collections::HashMap;
use std::ops::Range;
use unicode_segmentation::UnicodeSegmentation;

use crate::token::{TokenVec, TokenKind};
use crate::source_info::SourceRange;
use crate::error::Error;

#[inline]
pub fn lex<'src>(src: &'src str, lines: &'src mut Vec<usize>) -> (TokenVec, Vec<Error>) {
    Lexer::lex(src, lines)
}

struct Lexer<'src> {
    src: &'src str,
    gr: Vec<(usize, &'src str)>,
    pos: usize,
    tok_start_pos: usize,
    lines: &'src mut Vec<usize>,
    toks: TokenVec,
}


impl<'src> Lexer<'src> {
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
    /// to token tree, then sets `tok_start_pos` to `pos`.
    fn push(&mut self, kind: TokenKind) {
        let range = self.gr_range_to_src_range(self.tok_start_pos..self.pos);
        self.tok_start_pos = self.pos;
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
            src,
            gr: UnicodeSegmentation::grapheme_indices(src, true).collect(),
            pos: 0,
            tok_start_pos: 0,
            lines,
            toks: TokenVec::new(),
        };
        
        let mut errs = Vec::new();

        // This was in the old lexer, but I don't know why?
        while l.has_chars() {
            if l.is('\n') {
                l.lines.push(l.pos + 1);
            } else if l.is_str("\r\n") {
                l.pos += 1;
                l.lines.push(l.pos + 1);
            } else if l.is('\r') {
                l.lines.push(l.pos + 1);
            }
            l.pos += 1;
        }
        l.pos = 0;

        loop {
            // EOF.
            if l.pos == l.gr.len() {
                l.push(TokenKind::Eof);
                break;
            }

            // Newlines.
            let mut found_tok = false;
            while l.has_chars() && l.is_newline() { 
                found_tok = true;
                l.pos += 1; 
            }
            if found_tok { l.push(TokenKind::Newline); }

            // Whitespace.
            found_tok = false;
            while l.has_chars() && l.is_whitespace() {
                found_tok = true;
                l.pos += 1;
            }
            if found_tok { l.push(TokenKind::Whitespace); }

            // Single-line comments.
            if l.is_str("//") {
                l.pos += 2;
                while l.has_chars() && !l.is_newline() {
                    l.pos += 1;
                }
                l.push(TokenKind::SingleLineComment);
            }

            // Multi-line comments.
            if l.is_str("/*") {
                let mut comment_begin = l.pos;
                let mut levels = 1;
                l.pos += 2;
                let mut prev_ending_delimiter = None;
                while l.has_chars() {
                    if l.is_str("/*") {
                        levels += 1;
                        comment_begin = l.pos;
                        l.pos += 1;
                    } else if l.is_str("*/") {
                        prev_ending_delimiter = Some(l.pos);
                        levels -= 1;
                        l.pos += 1;
                        assert!(levels >= 0);
                        if levels == 0 {
                            l.pos += 1;
                            break;
                        }
                    }
                    l.pos += 1;
                }
                if levels > 0 {
                    let mut err = Error::new(
                        "unterminated '/*' comment"
                    ).adding_primary_range(
                        l.gr_range_to_src_range(comment_begin..l.pos),
                        "previous '/*' delimiter here"
                    );
                    if let Some(prev_ending_delimiter) = prev_ending_delimiter {
                        err.add_primary_range(l.gr_range_to_src_range(prev_ending_delimiter..(prev_ending_delimiter + 2)), "previous '*/' delimiter here");
                        // Reset the position to the position right after the previous ending delimiter so we can keep lexing.
                        // This might be a terrible idea, we'll just have to wait and see.
                        l.pos = prev_ending_delimiter + 2;
                    }
                    errs.push(err);
                }
                l.push(TokenKind::MultiLineComment);
            }
            if l.is_str("*/") {
                l.pos += 2;
                errs.push(
                    Error::new(
                        "unexpected '*/' delimiter"
                    ).adding_primary_range(
                        l.gr_range_to_src_range(l.tok_start_pos..l.pos),
                        "no previous '/*' to match"
                    )
                );
            }

            macro_rules! match_tokens {
                ($($kind: ident $symbol: expr)+) => {
                    if false {}
                    $(
                        else if l.is_str($symbol) {
                            if $symbol == "/" {
                                println!("range: {:?}", l.gr_range_to_src_range(l.tok_start_pos..l.pos));
                            }
                            l.pos += UnicodeSegmentation::graphemes($symbol, true).count();
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
                l.pos += 1;
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
                                    l.pos..(l.pos + 1), 
                                    "escaped here"
                                )
                            );
                            l.cur()
                        }
                    } else {
                        // This is same expression that l.cur() returns, but calling that method makes
                        // the borrow checker complain. :(
                        l.gr[l.pos].1
                    };

                    match char_to_insert {
                        "\\" => {
                            l.pos += 1;
                            in_escape_mode = true;
                        },
                        "\"" => {
                            l.pos += 1;
                            terminated = true;
                            break;
                        },
                        char_to_insert => {
                            lit += char_to_insert;
                            l.pos += 1;
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
                            l.gr_index_to_src_index(l.tok_start_pos)..l.gr_index_to_src_index(l.tok_start_pos + 1), 
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
            if l.has_chars() && (l.is_letter() || l.is('_')) {
                loop {
                    text += l.cur();
                    l.pos += 1;

                    if !l.has_chars() || (!l.is_letter() && !l.is('_') && !l.is_num()) {
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
            if l.has_chars() && l.is_num() {
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
                    if !l.has_chars() || (!l.is_num() && !l.is('.')) {
                        break;
                    }
                    l.pos += 1;
                }

                let lit = &l.src[l.gr_range_to_src_range(l.tok_start_pos..l.pos)];
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