use core::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::lcgs::ast::BinaryOpKind::*;

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
    pub name: String,
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
