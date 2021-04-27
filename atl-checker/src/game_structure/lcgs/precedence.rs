use crate::game_structure::lcgs::ast::BinaryOpKind;
use crate::game_structure::lcgs::ast::BinaryOpKind::*;
use crate::game_structure::lcgs::precedence::Associativity::LeftToRight;

/// Describes the precedence of binary operators. Higher precedence means higher priority.
/// E.g. in "1 + 2 * 3" we have that * has higher precedence than +, and therefore it is
/// equivalent to "1 + (2 * 3)" and not "(1 + 2) * 3"
#[derive(Debug, Eq, PartialEq)]
pub struct Precedence(pub i32, pub Associativity);

/// Operator associativity. Describes if "1 + 2 + 3 + 4" means
/// "1 + (2 + (3 + 4))" (`RightToLeft`) or "((1 + 2) + 3) + 4" (`LeftToRight`).
/// `LeftToRight` is most common, and if it doesn't matter we also use `LeftToRight`.
#[derive(Debug, Eq, PartialEq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
}

/// Returns the precedence of a binary operator. Higher precedence means higher priority.
/// E.g. in "1 + 2 * 3" we have that * has higher precedence than +, and therefore it is
/// equivalent to "1 + (2 * 3)" and not "(1 + 2) * 3".
pub fn precedence(op: &BinaryOpKind) -> Precedence {
    match op {
        Multiplication | Division => Precedence(20, LeftToRight),
        Addition | Subtraction => Precedence(19, LeftToRight),
        LessThan | GreaterThan | LessOrEqual | GreaterOrEqual => Precedence(18, LeftToRight),
        Equality | Inequality => Precedence(17, LeftToRight),
        And => Precedence(16, LeftToRight),
        Or => Precedence(15, LeftToRight),
        Xor => Precedence(14, LeftToRight),
        Implication => Precedence(13, LeftToRight),
    }
}