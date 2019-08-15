use std::ffi::CString;

use crate::source_info::SourceRange;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind<'src> {
    Ident(&'src str),

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

    // Symbols
    Colon,
    Comma,
    LeftParen,
    RightParen,
    Dot,
    OpenCurly,
    CloseCurly,

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
pub struct TokenVec<'src> {
    pub kinds: Vec<TokenKind<'src>>,
    pub ranges: Vec<SourceRange>,
}

#[derive(Debug)]
pub struct Token<'src> {
    pub kind: &'src TokenKind<'src>,
    pub range: &'src SourceRange,
}

impl<'src> TokenVec<'src> {
    pub fn new() -> Self {
        Self {
            kinds: Vec::new(),
            ranges: Vec::new(),
        }
    }

    pub fn push(&mut self, kind: TokenKind<'src>, range: SourceRange) {
        self.kinds.push(kind);
        self.ranges.push(range);
    }

    pub fn at(&'src self, i: usize) -> Token<'src> {
        Token::<'src> {
            kind: &self.kinds[i],
            range: &self.ranges[i],
        }
    }

    pub fn len(&self) -> usize { self.kinds.len() }
}

impl<'src> TokenKind<'src> {
    pub fn is_insignificant(&self) -> bool {
        use TokenKind::*;
        match self {
            Whitespace | SingleLineComment | MultiLineComment | Newline => true,
            _ => false,
        }
    }

    pub fn is_significant(&self) -> bool { !self.is_insignificant() }
}