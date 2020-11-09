use std::rc::Rc;

#[derive(Debug, Eq, PartialEq)]
pub struct Root {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
}

#[derive(Debug, Eq, PartialEq)]
pub enum DeclKind {
    Const(Rc<ConstDecl>),
    Label(Rc<LabelDecl>),
    StateVar(Rc<StateVarDecl>),
    Player(Rc<PlayerDecl>),
    Module(Rc<TemplateDecl>),
    Transition(Rc<TransitionDecl>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Identifier {
    pub owner: Option<String>,
    pub name: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ConstDecl {
    pub name: Identifier,
    pub definition: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LabelDecl {
    pub guard: Expr,
    pub name: Identifier,
}

#[derive(Debug, Eq, PartialEq)]
pub struct PlayerDecl {
    pub name: Identifier,
    pub module: Identifier,
    pub relabelling: Relabelling,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Relabelling {
    pub relabellings: Vec<RelabelCase>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct RelabelCase {
    pub prev_name: Identifier,
    pub new_name: Identifier,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TemplateDecl {
    pub name: Identifier,
    pub decls: Vec<Decl>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StateVarDecl {
    pub name: Identifier,
    pub range: TypeRange,
    pub initial_value: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeRange {
    pub min: Expr,
    pub max: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TransitionDecl {
    pub name: Identifier,
    pub condition: Expr,
    pub state_changes: Vec<StateChange>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StateChange {
    pub name: Identifier,
    pub new_value: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExprKind {
    Number(i32),
    Ident(Rc<Identifier>),
    Negation(Rc<Expr>),
    UnaryOp(UnaryOpKind, Rc<Expr>),
    BinaryOp(BinaryOpKind, Rc<Expr>, Rc<Expr>),
    TernaryIf(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    BoolToNumConversion(Rc<Expr>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOpKind {
    LogicalNegation,
    Negation, // eg -4
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOpKind {
    Addition,
    Multiplication,
    Subtraction,
    Division,
    Equality, // Also serves as bi-implication
    Inequality,
    GreaterThan,
    LessThan,
    GreaterOrEqual,
    LessOrEqual,
    And,
    Or,
    Xor,
    Implication,
}
