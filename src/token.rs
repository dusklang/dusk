use std::ffi::CString;

use string_interner::DefaultSymbol as Sym;

use mire::source_info::SourceRange;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Ident(Sym),

    IntLit(u64),
    DecLit(f64),
    StrLit(CString),
    CharLit(i8),

    // Special
    Eof,
    Whitespace,
    Newline,
    SingleLineComment,
    MultiLineComment,

    // Keywords
    Fn,
    Return,
    True,
    False,
    If,
    Else,
    While,
    As,
    Struct,
    Do,
    Mut,
    Module,
    Import,

    // Symbols
    Colon,
    Comma,
    LeftParen,
    RightParen,
    Dot,
    OpenCurly,
    CloseCurly,
    OpenSquareBracket,
    CloseSquareBracket,
    AtSign,

    // Operators
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    BitwiseOrAssign,
    BitwiseAndAssign,
    Add,
    Sub,
    Asterisk,
    Div,
    Mod,
    Equal,
    NotEqual,
    LTE,
    LT,
    GTE,
    GT,
    LogicalOr,
    LogicalAnd,
    LogicalNot,
    Assign,
    Ampersand,
    Pipe,
}

#[derive(Debug)]
pub struct TokenVec {
    pub kinds: Vec<TokenKind>,
    pub ranges: Vec<SourceRange>,
}

#[derive(Debug)]
pub struct Token<'src> {
    pub kind: &'src TokenKind,
    pub range: SourceRange,
}

impl TokenVec {
    pub fn new() -> Self {
        Self {
            kinds: Vec::new(),
            ranges: Vec::new(),
        }
    }

    pub fn push(&mut self, kind: TokenKind, range: SourceRange) {
        self.kinds.push(kind);
        self.ranges.push(range);
    }

    pub fn at<'src>(&'src self, i: usize) -> Token<'src> {
        Token {
            kind: &self.kinds[i],
            range: self.ranges[i],
        }
    }

    pub fn len(&self) -> usize { self.kinds.len() }
}

impl TokenKind {
    pub fn is_insignificant(&self) -> bool {
        use TokenKind::*;
        matches!(self, Whitespace | SingleLineComment | MultiLineComment | Newline)
    }

    pub fn is_significant(&self) -> bool { !self.is_insignificant() }

    pub fn could_begin_expression(&self) -> bool {
        use TokenKind::*;
        !matches!(
            self,
            Eof | Whitespace | Newline | SingleLineComment | MultiLineComment | Fn | Else | As |
            Mut | Colon | Comma | RightParen | Dot | OpenCurly | CloseCurly | AddAssign |
            SubAssign | MultAssign | DivAssign | ModAssign | BitwiseOrAssign | BitwiseAndAssign |
            Div | Mod | Equal | NotEqual | LTE | LT | GTE | GT | LogicalOr | LogicalAnd | Assign |
            Pipe | AtSign | OpenSquareBracket | CloseSquareBracket
        )
    }
}