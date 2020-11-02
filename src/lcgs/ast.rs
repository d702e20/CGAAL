
struct LazyConcurrentGameStructure {
    pub players: Vec<Identifier>,
    pub state_vars: Vec<StateVariable>,
    pub vars: Vec<VariableDefinition>,
    pub transitions: Vec<Transition>
}

struct Identifier {
    label: String,
}

struct StateVariable {
    identifier: Identifier,
    size: i32,
    initial_value: i32,
}

struct VariableDefinition {
    identifier: Identifier,
    definition: Expr,
}

struct Transition {
    guard: Expr,
    choices: Vec<i32>,
    /// If None the resulting state is the source state
    result: Option<Vec<StateChange>>,
}

struct StateChange {
    state_var: Identifier,
    new_value: Expr,
}

struct Expr {
    kind: ExprKind,
}

enum ExprKind {
    Identifier(Identifier),
    Negation(Box<Expr>),
    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),
    TernaryIf(Box<Expr>, Box<Expr>, Box<Expr>)
}

enum UnaryOpKind {
    LogicalNegation,
    Negation, // eg -4
}

enum BinaryOpKind {
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