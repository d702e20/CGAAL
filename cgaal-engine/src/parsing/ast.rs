use crate::game_structure::Proposition;
use crate::parsing::span::Span;
use std::sync::Arc;
use crate::parsing::token::TokenKind;

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Expr { span, kind }
    }

    pub fn new_error() -> Self {
        Expr::new(Span::new(0, 0), ExprKind::Error)
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    True,
    False,
    Paren(Arc<Expr>),
    Ident(String),
    Prop(Proposition),
    Unary(UnaryOpKind, Arc<Expr>),
    Binary(BinaryOpKind, Arc<Expr>, Arc<Expr>),
    Coalition(Coalition),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpKind {
    Not,

    // Temporal operators
    Next,
    Eventually,
    Invariantly,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOpKind {
    And,
    Or,

    // Temporal operators
    Until,
}

impl BinaryOpKind {
    pub fn associativity(&self) -> Associativity {
        match self {
            _ => Associativity::LeftToRight,
        }
    }

    pub fn is_right_associative(&self) -> bool {
        self.associativity() == Associativity::RightToLeft
    }

    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOpKind::Or => 0,
            BinaryOpKind::And => 1,
            BinaryOpKind::Until => 2,
        }
    }
}

impl TryFrom<TokenKind> for BinaryOpKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::AmpAmp => Ok(BinaryOpKind::And),
            TokenKind::PipePipe => Ok(BinaryOpKind::Or),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Coalition {
    pub span: Span,
    pub players: Vec<Expr>,
    pub kind: CoalitionKind,
    pub expr: Arc<Expr>,
}

impl Coalition {
    pub fn new(span: Span, players: Vec<Expr>, kind: CoalitionKind, expr: Arc<Expr>) -> Self {
        Coalition {
            span,
            players,
            kind,
            expr,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoalitionKind {
    Despite,
    Enforce,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
}