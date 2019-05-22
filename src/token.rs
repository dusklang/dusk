use crate::source_info::SourceRange;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // TODO: Interning!
    Ident(String),

    IntLit(u64),
    DecLit(f64),
    StrLit(String),
    CharLit(u8),

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
pub struct TokenVec {
    pub kinds: Vec<TokenKind>,
    pub ranges: Vec<SourceRange>,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: &'a TokenKind,
    pub range: &'a SourceRange,
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

    pub fn at(&self, i: usize) -> Token {
        Token {
            kind: &self.kinds[i],
            range: &self.ranges[i],
        }
    }

    pub fn len(&self) -> usize { self.kinds.len() }
}

impl TokenKind {
    pub fn is_insignificant(&self) -> bool {
        use TokenKind::*;
        match self {
            Whitespace | SingleLineComment | MultiLineComment | Newline => true,
            _ => false,
        }
    }

    pub fn is_significant(&self) -> bool { !self.is_insignificant() }
}