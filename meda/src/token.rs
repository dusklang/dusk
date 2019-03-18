use crate::source_info::SourceRange;

#[derive(Debug)]
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
    LeftCurly,
    RightCurly,
    Dot,

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
pub struct Token {
    pub kind: TokenKind,
    pub range: SourceRange,
}

impl Token {
    pub fn new(kind: TokenKind, range: SourceRange) -> Token {
        Self { kind, range }
    }

    pub fn is_insignificant(&self) -> bool {
        use TokenKind::*;
        match self.kind {
            Whitespace | SingleLineComment | MultiLineComment | Newline => true,
            _ => false,
        }
    }

    pub fn is_significant(&self) -> bool { !self.is_insignificant() }
}
