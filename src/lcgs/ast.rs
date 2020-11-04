pub struct LazyConcurrentGameStructure {
    pub players: Vec<Identifier>,
    pub state_vars: Vec<StateVariableDefinition>,
    pub vars: Vec<VariableDefinition>,
    pub propositions: Vec<Identifier>,
    pub proposition_mappings: Vec<PropositionMapping>,
    pub transitions: Vec<Transition>
}

pub struct Identifier {
    pub label: String,
}

pub struct StateVariableDefinition {
    pub identifier: Identifier,
    pub size: i32,
    pub initial_value: i32,
}

pub struct VariableDefinition {
    pub identifier: Identifier,
    pub definition: Expr,
}

pub struct PropositionMapping {
    pub guard: Expr,
    pub propositions: Vec<Identifier>
}

pub struct Transition {
    pub guard: Expr,
    pub choices: Vec<i32>,
    /// If None the resulting state is the source state
    pub result: Option<Vec<StateChange>>,
}

pub struct StateChange {
    pub state_var: Identifier,
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