use core::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::lcgs::ast::BinaryOpKind::*;

/// The root of a LCGS program.
#[derive(Debug, Eq, PartialEq)]
pub struct Root {
    /// Declarations in the global scope. The parser ensures that only
    /// the allowed declaration types are present (e.g. not transitions).
    pub decls: Vec<Decl>,
}

/// A declaration
#[derive(Debug, Eq, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
}

/// Every kind of declaration
#[derive(Debug, Eq, PartialEq)]
pub enum DeclKind {
    Const(Rc<ConstDecl>),
    Label(Rc<LabelDecl>),
    StateVar(Rc<StateVarDecl>),
    StateVarChange(Rc<StateVarChangeDecl>),
    Player(Rc<PlayerDecl>),
    Template(Rc<TemplateDecl>),
    Transition(Rc<TransitionDecl>),
}

/// An identifier with an optional owner, eg "`p1.health`". In this language we only ever
/// have two scopes, template or global. Templates are essentially player types, so once
/// instantiated, the player will own an instance of each declaration inside the template.
/// Hence, identifier inside expressions can refer to a specific owner (the name in front
/// of the dot in "`p1.health`". If the owner is omitted, the owner is either the current
/// template (if such declaration exists) or the global scope.
#[derive(Debug, Eq, PartialEq)]
pub struct OwnedIdentifier {
    /// The owner of the declaration, i.e. the name in from of the dot in "`p1.health`".
    /// None implies that the owner is the current template or global
    pub owner: Option<String>,
    pub name: String,
}

/// An identifier (with no explicit owner). This is typically the name of a declaration.
#[derive(Debug, Eq, PartialEq)]
pub struct Identifier {
    pub name: String,
}

/// A constant. E.g. "`const max_health = 1`"
#[derive(Debug, Eq, PartialEq)]
pub struct ConstDecl {
    pub name: Identifier,
    pub definition: Expr,
}

/// A label declaration. Labels are also called propositions. Example: "`label alive = health > 0`"
#[derive(Debug, Eq, PartialEq)]
pub struct LabelDecl {
    pub condition: Expr,
    pub name: Identifier,
}

/// A player declaration. A player based on a template with some optional relabelling.
/// E.g. "`player p1 = shooter [target1=p2, target2=p3]`"
#[derive(Debug, Eq, PartialEq)]
pub struct PlayerDecl {
    pub name: Identifier,
    pub template: Identifier,
    pub relabeling: Relabeling,
}

/// A list of relabeling cases. Relabeling means replacing name with another name or a
/// number. This allows the user to slightly tweak a template. It can be thought of
/// as passing arguments to a template.
#[derive(Debug, Eq, PartialEq)]
pub struct Relabeling {
    pub relabellings: Vec<RelabelCase>,
}

/// A relabeling case. Whenever the `prev_name` is found in the given template, it is
/// replaced with `new_name`.
#[derive(Debug, Eq, PartialEq)]
pub struct RelabelCase {
    pub prev_name: Identifier,
    pub new_name: Identifier,
}

/// A template declaration. Essentially a player type.
#[derive(Debug, Eq, PartialEq)]
pub struct TemplateDecl {
    pub name: Identifier,
    /// The parser ensures that only the allowed declaration kinds are present
    /// (not constants, players, or other templates)
    pub decls: Vec<Decl>,
    pub params: Vec<Param>,
}

/// A parameter to a template. I.e. something that must be relabeled.
#[derive(Debug, Eq, PartialEq)]
pub struct Param {
    pub name: Identifier,
    pub typ: ParamType,
}

/// A parameter type. It is only possible to relabel to new identifiers or integers.
#[derive(Debug, Eq, PartialEq)]
pub enum ParamType {
    IdentType(Identifier),
    IntType,
}

/// A variable declaration. The state of the CGS is the combination of all variables.
/// E.g. "`health : [0 .. max_health] init max_health`"
#[derive(Debug, Eq, PartialEq)]
pub struct StateVarDecl {
    pub name: Identifier,
    pub range: TypeRange,
    pub initial_value: Expr,
}

/// A variable-change declaration. In this declaration the user defines how a variable
/// changes based on the previous state and the actions taken.
#[derive(Debug, Eq, PartialEq)]
pub struct StateVarChangeDecl {
    pub name: Identifier,
    pub next_value: Expr,
}

/// A range for state variables.
#[derive(Debug, Eq, PartialEq)]
pub struct TypeRange {
    pub min: Expr,
    pub max: Expr,
}

/// A transition declaration defines what actions a player can take.
/// If the condition is not satisfied, then the player cannot take the action in the
/// current state. Transitions in a CGS is the combination of all
/// players' actions. Each player must have at least one action available to them.
#[derive(Debug, Eq, PartialEq)]
pub struct TransitionDecl {
    pub name: Identifier,
    pub condition: Expr,
}

/// An expression. Expressions are always of type integer.
#[derive(Debug, Eq, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
}

/// Every kind of expression
#[derive(Debug, Eq, PartialEq)]
pub enum ExprKind {
    Number(i32),
    OwnedIdent(Rc<OwnedIdentifier>),
    Negation(Rc<Expr>),
    UnaryOp(UnaryOpKind, Rc<Expr>),
    BinaryOp(BinaryOpKind, Rc<Expr>, Rc<Expr>),
    TernaryIf(Rc<Expr>, Rc<Expr>, Rc<Expr>),
}

/// Unary operators
#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOpKind {
    LogicalNegation,
    Negation, // eg -4
}

/// Binary operators
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

// TODO Binary operators consisting of multiple characters, e.g. "==" or "&&"
impl From<u8> for BinaryOpKind {
    fn from(op: u8) -> BinaryOpKind {
        match op {
            b'+' => Addition,
            b'*' => Multiplication,
            b'-' => Subtraction,
            b'/' => Division,
            _ => unimplemented!(
                "Unrecognized operator '{}'. See 'impl From<u8> for BinaryOpKind' clause.",
                op
            ),
        }
    }
}

impl Display for BinaryOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Addition => "+",
                Multiplication => "*",
                Subtraction => "-",
                Division => "/",
                Equality => "==",
                Inequality => "!=",
                GreaterThan => ">",
                LessThan => "<",
                GreaterOrEqual => ">=",
                LessOrEqual => "<=",
                And => "&&",
                Or => "||",
                Xor => "^",
                Implication => "->",
            }
        )
    }
}
