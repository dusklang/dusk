use std::ffi::CString;

use string_interner::DefaultSymbol as Sym;

use crate::source_info::SourceRange;

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
    OpenGenerics,
    CloseGenerics,
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
    pub fn is_whitespace(&self) -> bool {
        use TokenKind::*;
        matches!(self, Whitespace | SingleLineComment | MultiLineComment | Newline)
    }

    pub fn could_begin_expression(&self) -> bool {
        use TokenKind::*;
        !matches!(
            self,
            Eof | Whitespace | Newline | SingleLineComment | MultiLineComment | Else | As |
            Mut | Comma | RightParen | OpenCurly | CloseCurly | AddAssign | SubAssign |
            MultAssign | DivAssign | ModAssign | BitwiseOrAssign | BitwiseAndAssign |
            Div | Mod | Equal | NotEqual | Lte | Lt | Gte | GT | LogicalOr | LogicalAnd | Assign |
            Pipe | OpenSquareBracket | CloseSquareBracket | OpenGenerics | CloseGenerics
        )
    }

    pub fn could_begin_struct_literal_field(&self) -> bool {
        matches!(self, TokenKind::Ident(_))
    }

    pub fn could_begin_struct_field(&self) -> bool {
        // attributes on struct fields aren't supported yet, but probably will be.
        matches!(self, TokenKind::Ident(_) | TokenKind::AtSign)
    }

    pub fn could_begin_parameter(&self) -> bool {
        // attributes on parameters aren't supported yet, but probably will be.
        matches!(self, TokenKind::Ident(_) | TokenKind::AtSign)
    }

    pub fn could_begin_variant_decl(&self) -> bool {
        // attributes on enum variants aren't supported yet, but probably will be.
        matches!(self, TokenKind::Ident(_) | TokenKind::AtSign)
    }

    pub fn could_begin_pattern(&self) -> bool {
        matches!(self, TokenKind::Dot | TokenKind::Ident(_) | TokenKind::IntLit(_))
    }

    pub fn could_begin_statement(&self) -> bool {
        self.could_begin_expression() || 
            self.could_begin_pattern() || 
            matches!(self, TokenKind::AtSign | TokenKind::Ident(_) | TokenKind::Mut | TokenKind::Fn)
    }

    pub fn could_begin_generic_parameter(&self) -> bool {
        matches!(self, TokenKind::Ident(_))
    }

    pub fn pretty_print_separator(&self) -> Option<&'static str> {
        match self {
            TokenKind::Comma => Some(","),
            TokenKind::Semicolon => Some(";"),
            _ => None,
        }
    }
}
