use crate::parsing::span::Span;
use crate::parsing::token::TokenKind;
use std::sync::Arc;

/// The root of an LCGS program.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LcgsRoot {
    pub span: Span,
    pub items: Vec<Decl>,
}

impl LcgsRoot {
    pub fn new(span: Span, items: Vec<Decl>) -> Self {
        LcgsRoot { span, items }
    }
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
    /// A constant declaration
    Const(Arc<Expr>),
    /// A state label declaration. Used by ATL formulas
    StateLabel(Arc<Expr>),
    /// A state variable declaration. These compose the state of a game.
    StateVar(Arc<StateVarDecl>),
    /// A player declaration. Can only appear in the global scope.
    Player(Arc<PlayerDecl>),
    /// A template declaration. Can only appear in the global scope.
    Template(Vec<Decl>),
    /// An action declaration. Can only appear in templates.
    Action(Arc<Expr>),
    /// An error
    Error,
}

impl Decl {
    pub fn new(span: Span, ident: Ident, kind: DeclKind) -> Self {
        Decl { span, ident, kind }
    }

    pub fn new_error() -> Self {
        Decl::new(Span::new(0, 0), Ident::new_error(), DeclKind::Error)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StateVarDecl {
    pub range: RangeClause,
    pub init: Arc<Expr>,
    pub update_ident: Ident,
    pub update: Arc<Expr>,
}

impl StateVarDecl {
    pub fn new(
        range: RangeClause,
        init: Arc<Expr>,
        update_ident: Ident,
        update: Arc<Expr>,
    ) -> Self {
        StateVarDecl {
            range,
            init,
            update_ident,
            update,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RangeClause {
    pub span: Span,
    pub min: Arc<Expr>,
    pub max: Arc<Expr>,
}

impl RangeClause {
    pub fn new(span: Span, min: Arc<Expr>, max: Arc<Expr>) -> Self {
        RangeClause { span, min, max }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PlayerDecl {
    pub template: Ident,
    pub relabellings: Vec<RelabelCase>,
}

impl PlayerDecl {
    pub fn new(template: Ident, relabellings: Vec<RelabelCase>) -> Self {
        PlayerDecl {
            template,
            relabellings,
        }
    }
}

/// A relabelling case, as found in player declarations.
/// Every occurrence of the `from` identifier in the template will be replaced by the `to` expression.
/// If the ident is the name of a declaration or a name with an owner (e.g. `foo` in `p1.foo`),
/// then the expression must be an identifier too.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RelabelCase {
    pub span: Span,
    pub from: Ident,
    pub to: Arc<Expr>,
}

impl RelabelCase {
    pub fn new(span: Span, from: Ident, to: Arc<Expr>) -> Self {
        RelabelCase { span, from, to }
    }
}

/// An identifier.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

impl Ident {
    pub fn new(span: Span, name: String) -> Self {
        Ident { span, name }
    }

    pub fn new_error() -> Self {
        Ident::new(Span::new(0, 0), String::new())
    }
}

/// An owned identifier. E.g. `p1.foo`
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
