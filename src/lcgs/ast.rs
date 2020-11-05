pub struct Root { // TODO
    pub players: Vec<Identifier>,
    pub state_vars: Vec<StateVariableDefinition>,
    pub vars: Vec<VariableDefinition>,
    pub propositions: Vec<Identifier>,
    pub proposition_mappings: Vec<PropositionMapping>,
    pub transitions: Vec<Transition>
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
    pub relabellings: Vec<Relabel>,
}

pub struct Relabel {
    pub prev_name: Identifier,
    pub new_name: Identifier,
}

pub struct ModuleDecl {
    pub name: Identifier,
    pub statements: Vec<ModuleDeclStatement>,
}

pub struct ModuleDeclStatement {
    pub kind: ModuleDeclStatementKind,
}

pub enum ModuleDeclStatementKind {
    StateVarDecl(Box<StateVarDecl>),
    TransitionDecl(Box<TransitionDecl>),
}

pub struct StateVarDecl; // TODO

pub struct TransitionDecl; // TODO

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