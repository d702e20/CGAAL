use crate::parsing::span::Span;
use std::fmt::{Debug, Display, Formatter};

/// A single token in the input.
#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    /// The span of the token in the original input code.
    pub span: Span,
    /// The kind of the token.
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Tokens are used print as their kind
        write!(f, "{}", self.kind)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'{}", self.kind, self.span)
    }
}

/// The kind of a token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Delimiters
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Langle,
    Rangle,
    Lbracket,
    Rbracket,
    Llangle,
    Rrangle,
    Llbracket,
    Rrbracket,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Neq,
    Geq,
    Leq,
    Bang,
    AmpAmp,
    PipePipe,
    Hat,
    Arrow,
    Question,

    // Other symbols
    Assign,
    Comma,
    Dot,
    DotDot,
    Semi,
    Colon,
    Prime,

    // Keywords
    KwConst,
    KwLabel,
    KwPlayer,
    KwTemplate,
    KwEndTemplate,
    KwInit,
    KwMin,
    KwMax,
    True,
    False,

    // Literals
    Num(i32),
    Word(String),

    // Error
    Err(String),
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
            TokenKind::Lbracket => write!(f, "["),
            TokenKind::Rbracket => write!(f, "]"),
            TokenKind::Llangle => write!(f, "<<"),
            TokenKind::Rrangle => write!(f, ">>"),
            TokenKind::Llbracket => write!(f, "[["),
            TokenKind::Rrbracket => write!(f, "]]"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::Neq => write!(f, "!="),
            TokenKind::Geq => write!(f, ">="),
            TokenKind::Leq => write!(f, "<="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::AmpAmp => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::Hat => write!(f, "^"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Prime => write!(f, "'"),
            TokenKind::KwConst => write!(f, "const"),
            TokenKind::KwLabel => write!(f, "label"),
            TokenKind::KwPlayer => write!(f, "player"),
            TokenKind::KwTemplate => write!(f, "template"),
            TokenKind::KwEndTemplate => write!(f, "endtemplate"),
            TokenKind::KwInit => write!(f, "init"),
            TokenKind::KwMin => write!(f, "min"),
            TokenKind::KwMax => write!(f, "max"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Num(n) => write!(f, "{n}"),
            TokenKind::Word(w) => write!(f, "{w}"),
            TokenKind::Err(e) => write!(f, "{e}"),
        }
    }
}
