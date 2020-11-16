use std::rc::Rc;

pub struct Root {
    pub decls: Vec<Decl>,
}

pub struct Decl {
    pub kind: DeclKind,
}

pub enum DeclKind {
    Const(Rc<ConstDecl>),
    Label(Rc<LabelDecl>),
    StateVar(Rc<StateVarDecl>),
    Player(Rc<PlayerDecl>),
    Module(Rc<TemplateDecl>),
    Transition(Rc<TransitionDecl>),
}

pub struct Identifier {
    pub owner: Option<String>,
    pub name: String,
}

pub struct ConstDecl {
    pub name: Identifier,
    pub definition: Expr,
}

pub struct LabelDecl {
    pub guard: Expr,
    pub name: Identifier,
}

pub struct PlayerDecl {
    pub name: Identifier,
    pub module: Identifier,
    pub relabelling: Relabelling,
}

pub struct Relabelling {
    pub relabellings: Vec<RelabelCase>,
}

pub struct RelabelCase {
    pub prev_name: Identifier,
    pub new_name: Identifier,
}

pub struct TemplateDecl {
    pub name: Identifier,
    pub decls: Vec<Decl>,
    pub params: Vec<Params>,
}

pub struct StateVarDecl {
    pub name: Identifier,
    pub range: TypeRange,
    pub initial_value: Expr,
}

pub struct TypeRange {
    pub min: Expr,
    pub max: Expr,
}

pub struct Params {
    pub name: Identifier,
}

pub struct TransitionDecl {
    pub name: Identifier,
    pub condition: Expr,
    pub state_changes: Vec<StateChange>,
}

pub struct StateChange {
    pub name: Identifier,
    pub new_value: Expr,
}

pub struct Expr {
    pub kind: ExprKind,
}

pub enum ExprKind {
    Number(i32),
    Identifier(Identifier),
    Negation(Rc<Expr>),
    UnaryOp(UnaryOpKind, Rc<Expr>),
    BinaryOp(BinaryOpKind, Rc<Expr>, Rc<Expr>),
    TernaryIf(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    BoolToNumConversion(Rc<Expr>),
}

pub enum UnaryOpKind {
    LogicalNegation,
    Negation, // eg -4
}

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
