use crate::source_info::SourceRange;

#[derive(Debug)]
pub enum TokenKind<'src> {
    Ident(&'src str),
    IntLit(u64),
    DecLit(f64),
    StrLit(&'src str),
    CharLit(i8),

    // Special
    Eof,
    Whitespace,
    Newline,
    SingleLineComment,
    MultiLineComment,

    // Keywords
    Def,
    Var,
    Extern,
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

pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub range: SourceRange,
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind<'src>, range: SourceRange) -> Token<'src> {
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
