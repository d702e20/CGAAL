use crate::parsing::span::Span;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    span: Span,
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    #[allow(unused)]
    pub fn span(&self) -> &Span {
        &self.span
    }

    #[allow(unused)]
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'{}", self.kind, self.span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Delimiters
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Langle,
    Rangle,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    EqEq,
    Neq,
    Geq,
    Leq,

    // Other symbols
    Eq,

    // Literals
    Num(i32),
    Word(String),

    // Utility
    Err,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Lparen => write!(f, "("),
            TokenKind::Rparen => write!(f, ")"),
            TokenKind::Lbrace => write!(f, "{{"),
            TokenKind::Rbrace => write!(f, "}}"),
            TokenKind::Langle => write!(f, "<"),
            TokenKind::Rangle => write!(f, ">"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::Neq => write!(f, "!="),
            TokenKind::Geq => write!(f, ">="),
            TokenKind::Leq => write!(f, "<="),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Num(n) => write!(f, "{n}"),
            TokenKind::Word(w) => write!(f, "{w}"),
            TokenKind::Err => write!(f, "@err"),
        }
    }
}
