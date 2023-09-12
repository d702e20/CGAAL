use crate::parsing::span::Span;
use crate::parsing::token::TokenKind;
use std::sync::Arc;

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    /// The span of the expression in the original input code.
    pub span: Span,
    /// The kind of the expression.
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    /// The true constant
    True,
    /// The false constant
    False,
    /// An expression in parentheses
    Paren(Arc<Expr>),
    /// An identifier
    Ident(String),
    /// A unary operation
    Unary(UnaryOpKind, Arc<Expr>),
    /// A binary operation
    Binary(BinaryOpKind, Arc<Expr>, Arc<Expr>),
    /// An expression with a coalition
    Coalition(Coalition),
    /// An error
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpKind {
    /// The `!` operator for negation
    Not,

    // Temporal operators
    /// The `X` temporal operator (neXt)
    Next,
    /// The `F` temporal operator (Future/Eventually)
    Eventually,
    /// The `G` temporal operator (Globally/Invariant)
    Invariantly,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOpKind {
    /// The `&&` operator for logical and (conjunction)
    And,
    /// The `||` operator for logical or (disjunction)
    Or,
    /// The `.` operator for owned symbols
    Dot,

    // Temporal operators
    /// The `U` temporal operator (Until)
    Until,
}

impl BinaryOpKind {
    /// Returns the associativity of the operator.
    /// E.g. `-` is left-associative so `a - b - c` is parsed as `(a - b) - c`.
    pub fn associativity(&self) -> Associativity {
        // All operators are left-associative so far
        Associativity::LeftToRight
    }

    pub fn is_right_associative(&self) -> bool {
        self.associativity() == Associativity::RightToLeft
    }

    /// Returns the precedence of the operator.
    /// Higher precedence means the operator binds tighter.
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOpKind::Dot => 3,
            BinaryOpKind::And => 2,
            BinaryOpKind::Or => 1,
            BinaryOpKind::Until => 0,
        }
    }
}

impl TryFrom<TokenKind> for BinaryOpKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::AmpAmp => Ok(BinaryOpKind::And),
            TokenKind::PipePipe => Ok(BinaryOpKind::Or),
            _ => Err(()),
        }
    }
}

/// The associativity of an operator.
/// E.g. `-` is left-associative so `a - b - c` is parsed as `(a - b) - c`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
}

/// A coalition expression. E.g. `<< p1 >> path_expr`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Coalition {
    /// The span of the coalition in the original input code.
    pub span: Span,
    /// The players in the coalition.
    pub players: Vec<Expr>,
    /// The kind of the coalition.
    pub kind: CoalitionKind,
    /// The path expression following the coalition.
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
    /// The `<< >>` coalition
    Despite,
    /// The `[[ ]]` coalition
    Enforce,
}
