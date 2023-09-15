use crate::parsing::span::Span;
use crate::parsing::token::TokenKind;
use std::sync::Arc;

/// The root of an LCGS program.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LcgsRoot {
    pub span: Span,
    pub items: Vec<Decl>,
}

/// A declaration.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Decl {
    pub span: Span,
    pub ident: Ident,
    pub kind: DeclKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DeclKind {
    Const(Arc<Expr>),
    StateLabel(Arc<Expr>),
    StateVar(Arc<StateVarDecl>),
    Player(Arc<PlayerDecl>),
    Template(Vec<Decl>),
    Action(Arc<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StateVarDecl {
    pub init: Arc<Expr>,
    pub update: Arc<Expr>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RangeClause {
    pub span: Span,
    pub min: Arc<Expr>,
    pub max: Arc<Expr>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PlayerDecl {
    pub template: Ident,
    pub relabellings: Vec<RelabelCase>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RelabelCase {
    pub span: Span,
    pub from: Ident,
    pub to: Arc<Expr>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

impl Ident {
    pub fn new(span: Span, name: String) -> Self {
        Ident { span, name }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OwnedIdent {
    pub owner: Ident,
    pub name: Ident,
}

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
    /// An owned identifier
    OwnedIdent(Option<Ident>, Ident),
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
    pub players: Vec<Ident>,
    /// The kind of the coalition.
    pub kind: CoalitionKind,
    /// The path expression following the coalition.
    pub expr: Arc<Expr>,
}

impl Coalition {
    pub fn new(span: Span, players: Vec<Ident>, kind: CoalitionKind, expr: Arc<Expr>) -> Self {
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
