use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, RangeInclusive, Sub};
use crate::game_structure::lcgs::symbol_table::SymbIdx;
use crate::game_structure::{INVALID_IDX, PlayerIdx, PropIdx};
use crate::parsing::span::{NO_SPAN, Span};
use crate::parsing::token::TokenKind;

/// The root of an LCGS program.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LcgsRoot {
    pub span: Span,
    pub decls: Vec<Decl>,
}

impl LcgsRoot {
    pub fn new(span: Span, decls: Vec<Decl>) -> Self {
        LcgsRoot { span, decls }
    }
}

/// A declaration.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Decl {
    pub span: Span,
    pub ident: OwnedIdent,
    pub kind: DeclKind,
}

impl Decl {
    pub fn new(span: Span, ident: Ident, kind: DeclKind) -> Self {
        Decl { span, ident: OwnedIdent::new(None, ident), kind }
    }

    pub fn new_error() -> Self {
        Decl::new(NO_SPAN, Ident::new_error(), DeclKind::Error)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DeclKind {
    /// A constant declaration
    Const(Expr),
    /// A state label declaration. Used by ATL formulas
    StateLabel(PropIdx, Expr),
    /// A state variable declaration. These compose the state of a game.
    StateVar(StateVarDecl),
    /// A player declaration. Can only appear in the global scope.
    Player(PlayerDecl),
    /// A template declaration. Can only appear in the global scope.
    Template(Vec<Decl>),
    /// An action declaration. Can only appear in templates.
    Action(Expr),
    /// An error
    Error,
}

impl DeclKind {
    pub fn is_const(&self) -> bool {
        matches!(self, DeclKind::Const(_))
    }

    pub fn is_state_label(&self) -> bool {
        matches!(self, DeclKind::StateLabel(_, _))
    }

    pub fn is_state_var(&self) -> bool {
        matches!(self, DeclKind::StateVar(_))
    }

    pub fn is_player(&self) -> bool {
        matches!(self, DeclKind::Player(_))
    }

    pub fn is_template(&self) -> bool {
        matches!(self, DeclKind::Template(_))
    }

    pub fn is_action(&self) -> bool {
        matches!(self, DeclKind::Action(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self, DeclKind::Error)
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            DeclKind::Const(_) => "constant",
            DeclKind::StateLabel(_, _) => "state label",
            DeclKind::StateVar(_) => "state variable",
            DeclKind::Player(_) => "player",
            DeclKind::Template(_) => "template",
            DeclKind::Action(_) => "action",
            DeclKind::Error => "error",
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StateVarDecl {
    pub range: RangeClause,
    pub init: Expr,
    pub init_val: i32,
    pub update_ident: Ident,
    pub update: Expr,
}

impl StateVarDecl {
    pub fn new(
        range: RangeClause,
        init: Expr,
        update_ident: Ident,
        update: Expr,
    ) -> Self {
        StateVarDecl {
            range,
            init,
            init_val: 0,
            update_ident,
            update,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RangeClause {
    pub span: Span,
    pub min: Expr,
    pub max: Expr,
    pub val: RangeInclusive<i32>,
}

impl RangeClause {
    pub fn new(span: Span, min: Expr, max: Expr) -> Self {
        RangeClause { span, min, max, val: 0..=0 }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PlayerDecl {
    pub index: PlayerIdx,
    pub template_ident: Ident,
    pub relabellings: Vec<RelabelCase>,
}

impl PlayerDecl {
    pub fn new(template: Ident, relabellings: Vec<RelabelCase>) -> Self {
        PlayerDecl {
            index: PlayerIdx(INVALID_IDX),
            template_ident: template,
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
    pub to: Expr,
}

impl RelabelCase {
    pub fn new(span: Span, from: Ident, to: Expr) -> Self {
        RelabelCase { span, from, to }
    }
}

/// An identifier.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Ident {
    pub span: Span,
    pub text: String,
}

impl Ident {
    pub fn new(span: Span, name: String) -> Self {
        Ident { span, text: name }
    }

    pub fn new_error() -> Self {
        Ident::new(NO_SPAN, String::new())
    }

    pub fn with_owner(self, owner: Ident) -> OwnedIdent {
        OwnedIdent::new(Some(owner), self)
    }

    pub fn with_no_owner(self) -> OwnedIdent {
        OwnedIdent::new(None, self)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

/// An owned identifier. E.g. `p1.foo`
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OwnedIdent {
    pub owner: Option<Ident>,
    pub name: Ident,
}

impl OwnedIdent {
    pub fn new(owner: Option<Ident>, name: Ident) -> Self {
        OwnedIdent { owner, name: name }
    }

    pub fn new_error() -> Self {
        OwnedIdent::new(None, Ident::new_error())
    }
}

impl From<&str> for OwnedIdent {
    /// Converts a string into an [OwnedIdent].
    /// The string must be on the form "owner.name" or just "name".
    fn from(string: &str) -> OwnedIdent {
        let split: Vec<&str> = string.split('.').collect();
        match split.len() {
            1 => OwnedIdent::new(None, Ident::new(NO_SPAN, split[0].to_string())),
            2 => OwnedIdent::new(
                Some(Ident::new(NO_SPAN, split[0].to_string())),
                Ident::new(NO_SPAN, split[1].to_string()),
                ),
            _ => panic!("Invalid owned identifier. Must consist of an owner and a name."),
        }
    }
}

impl Display for OwnedIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(owner) = &self.owner {
            write!(f, "{}.{}", owner.text, self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
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
        Expr::new(NO_SPAN, ExprKind::Error)
    }

    pub fn is_true(&self) -> bool {
        self.kind == ExprKind::True
    }

    pub fn is_false(&self) -> bool {
        self.kind == ExprKind::False
    }

    pub fn is_num(&self) -> bool {
        matches!(self.kind, ExprKind::Num(_))
    }

    pub fn is_paren(&self) -> bool {
        matches!(self.kind, ExprKind::Paren(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, ExprKind::OwnedIdent(_))
    }

    pub fn is_unary(&self) -> bool {
        matches!(self.kind, ExprKind::Unary(_, _))
    }

    pub fn is_binary(&self) -> bool {
        matches!(self.kind, ExprKind::Binary(_, _, _))
    }

    pub fn is_ternary_if(&self) -> bool {
        matches!(self.kind, ExprKind::TernaryIf(_, _, _))
    }

    pub fn is_max(&self) -> bool {
        matches!(self.kind, ExprKind::Max(_))
    }

    pub fn is_min(&self) -> bool {
        matches!(self.kind, ExprKind::Min(_))
    }

    pub fn is_coalition(&self) -> bool {
        matches!(self.kind, ExprKind::Coalition(_))
    }

    pub fn is_error(&self) -> bool {
        self.kind == ExprKind::Error
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    /// The true constant
    True,
    /// The false constant
    False,
    Num(i32),
    /// An expression in parentheses
    Paren(Box<Expr>),
    /// An owned identifier
    OwnedIdent(OwnedIdent),
    /// A value of a symbol
    Symbol(SymbIdx),
    /// A unary operation
    Unary(UnaryOpKind, Box<Expr>),
    /// A binary operation
    Binary(BinaryOpKind, Box<Expr>, Box<Expr>),
    /// A ternary if expression
    TernaryIf(Box<Expr>, Box<Expr>, Box<Expr>),
    /// A max expression
    Max(Vec<Expr>),
    /// A min expression
    Min(Vec<Expr>),
    /// An expression with a coalition
    Coalition(Coalition),
    /// An error
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpKind {
    /// The `!` operator for logical negation
    Not,
    /// The `-` operator for arithmetic negation
    Neg,

    // Temporal operators
    /// The `X` temporal operator (neXt)
    Next,
    /// The `F` temporal operator (Future/Eventually)
    Eventually,
    /// The `G` temporal operator (Globally/Invariant)
    Invariantly,
}

impl UnaryOpKind {
    pub fn as_fn(&self) -> fn(i32) -> i32 {
        match self {
            UnaryOpKind::Not => |e| (e == 0) as i32,
            UnaryOpKind::Neg => |e| -e,
            UnaryOpKind::Next => panic!("Temporal operator (Next) is not a function"),
            UnaryOpKind::Eventually => panic!("Temporal operator (Eventually) is not a function"),
            UnaryOpKind::Invariantly => panic!("Temporal operator (Invariantly) is not a function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOpKind {
    // Logical operators
    /// The `&&` operator for logical and (conjunction)
    And,
    /// The `||` operator for logical or (disjunction)
    Or,
    /// The `^` operator for exclusive or (xor)
    Xor,
    /// The `->` operator for implication
    Implies,

    // Relational operators
    /// The `==` operator for equality
    Eq,
    /// The `!=` operator for inequality
    Neq,
    /// The `>` operator for greater than
    Gt,
    /// The `>=` operator for greater than or equal
    Geq,
    /// The `<` operator for less than
    Lt,
    /// The `<=` operator for less than or equal
    Leq,

    // Arithmetic operators
    /// The `+` operator for addition
    Add,
    /// The `-` operator for subtraction
    Sub,
    /// The `*` operator for multiplication
    Mul,
    /// The `/` operator for division
    Div,

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
            BinaryOpKind::Mul | BinaryOpKind::Div => 8,
            BinaryOpKind::Add | BinaryOpKind::Sub => 7,
            BinaryOpKind::Gt | BinaryOpKind::Geq | BinaryOpKind::Lt | BinaryOpKind::Leq => 6,
            BinaryOpKind::Eq | BinaryOpKind::Neq => 5,
            BinaryOpKind::And => 4,
            BinaryOpKind::Or => 3,
            BinaryOpKind::Xor => 2,
            BinaryOpKind::Implies => 1,
            BinaryOpKind::Until => 0,
        }
    }

    pub fn as_fn(&self) -> fn(i32, i32) -> i32 {
        match self {
            BinaryOpKind::Add => i32::add,
            BinaryOpKind::Mul => i32::mul,
            BinaryOpKind::Sub => i32::sub,
            BinaryOpKind::Div => i32::div,
            BinaryOpKind::Eq => |e1, e2| (e1 == e2) as i32,
            BinaryOpKind::Neq => |e1, e2| (e1 != e2) as i32,
            BinaryOpKind::Gt => |e1, e2| (e1 > e2) as i32,
            BinaryOpKind::Lt => |e1, e2| (e1 < e2) as i32,
            BinaryOpKind::Geq => |e1, e2| (e1 >= e2) as i32,
            BinaryOpKind::Leq => |e1, e2| (e1 <= e2) as i32,
            BinaryOpKind::And => |e1, e2| (e1 != 0 && e2 != 0) as i32,
            BinaryOpKind::Or => |e1, e2| (e1 != 0 || e2 != 0) as i32,
            BinaryOpKind::Xor => |e1, e2| ((e1 == 0 && e2 != 0) || e1 != 0 && e2 == 0) as i32,
            BinaryOpKind::Implies => |e1, e2| (e1 == 0 || e2 != 0) as i32,
            BinaryOpKind::Until => panic!("Temporal operator (Until) is not a function"),
        }
    }
}

impl TryFrom<TokenKind> for BinaryOpKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::AmpAmp => Ok(BinaryOpKind::And),
            TokenKind::PipePipe => Ok(BinaryOpKind::Or),
            TokenKind::Hat => Ok(BinaryOpKind::Xor),
            TokenKind::Arrow => Ok(BinaryOpKind::Implies),
            TokenKind::Eq => Ok(BinaryOpKind::Eq),
            TokenKind::Neq => Ok(BinaryOpKind::Neq),
            TokenKind::Rangle => Ok(BinaryOpKind::Gt),
            TokenKind::Geq => Ok(BinaryOpKind::Geq),
            TokenKind::Langle => Ok(BinaryOpKind::Lt),
            TokenKind::Leq => Ok(BinaryOpKind::Leq),
            TokenKind::Plus => Ok(BinaryOpKind::Add),
            TokenKind::Minus => Ok(BinaryOpKind::Sub),
            TokenKind::Star => Ok(BinaryOpKind::Mul),
            TokenKind::Slash => Ok(BinaryOpKind::Div),
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
    pub expr: Box<Expr>,
}

impl Coalition {
    pub fn new(span: Span, players: Vec<Ident>, kind: CoalitionKind, expr: Expr) -> Self {
        Coalition {
            span,
            players,
            kind,
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoalitionKind {
    /// The `<< >>` coalition
    Despite,
    /// The `[[ ]]` coalition
    Enforce,
}
