pub struct Root {
    decls: Vec<Decl>,
}

pub struct Decl {
    kind: DeclKind,
}

pub enum DeclKind {
    Const(Box<ConstDecl>),
    Label(Box<LabelDecl>),
    StateVar(Box<StateVarDecl>),
    Player(Box<PlayerDecl>),
    Module(Box<ModuleDecl>),
    Transition(Box<TransitionDecl>),
}

pub struct Identifier {
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

pub struct ModuleDecl {
    pub name: Identifier,
    pub decls: Vec<Decl>,
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
    Identifier(Identifier),
    Negation(Box<Expr>),
    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),
    TernaryIf(Box<Expr>, Box<Expr>, Box<Expr>),
    BoolToNumConversion(Box<Expr>),
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
    Implication
}