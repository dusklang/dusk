use std::ffi::CString;

use string_interner::DefaultSymbol as Sym;

use dusk_dire::source_info::SourceRange;

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
    Loop,
    For,
    Break,
    Continue,
    In,
    Switch,
    As,
    Struct,
    Enum,
    Do,
    Mut,
    Module,
    ExternModule,
    Import,
    DebugMark,

    // Symbols
    Colon,
    Semicolon,
    Comma,
    LeftParen,
    RightParen,
    DoubleDot,
    Dot,
    OpenCurly,
    CloseCurly,
    OpenSquareBracket,
    CloseSquareBracket,
    AtSign,
    ReturnArrow,

    // Operators
    Tilde,
    Caret,
    LeftShift,
    RightShift,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    BitwiseOrAssign,
    BitwiseAndAssign,
    XorAssign,
    LeftShiftAssign,
    RightShiftAssign,
    Add,
    Sub,
    Asterisk,
    Div,
    Mod,
    Equal,
    NotEqual,
    Lte,
    Lt,
    Gte,
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

impl Default for TokenVec {
    fn default() -> Self {
        Self::new()
    }
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
        let i = std::cmp::min(i, self.kinds.len()-1);
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
            Div | Mod | Equal | NotEqual | Lte | Lt | Gte | GT | LogicalOr | LogicalAnd | Assign |
            Pipe | AtSign | OpenSquareBracket | CloseSquareBracket
        )
    }

    pub fn pretty_print_separator(&self) -> Option<&'static str> {
        match self {
            TokenKind::Comma => Some(","),
            TokenKind::Semicolon => Some(";"),
            _ => None,
        }
    }
}