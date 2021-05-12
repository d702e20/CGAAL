use crate::game_structure::lcgs::ast::{BinaryOpKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use minilp::ComparisonOp;
use std::collections::HashMap;

/// Holds extracted linear expressions from the formula in AtlVertex
/// E.g. `0 = ax + by + cz + k`
#[derive(Clone)]
pub struct LinearConstraint {
    /// A list of variables and their coefficients
    pub terms: Vec<(SymbolIdentifier, i32)>,
    pub constant: i32,
    pub comparison: ComparisonOp,
    /// The norm of the coefficients. That is, if the linear expression is `ax + by + cz + k`, then
    /// this is `sqrt(a*a + b*b + c*c)`
    pub coefficient_norm: f64,
}

pub struct LinearConstraintExtractor {
    terms: HashMap<SymbolIdentifier, i32>,
    constant: i32,
    comparison: ComparisonOp,
}

impl LinearConstraintExtractor {
    pub fn extract(expr: &Expr) -> Option<LinearConstraint> {
        if let ExprKind::BinaryOp(operator, lhs, rhs) = &expr.kind {
            let comparison = match operator {
                BinaryOpKind::Equality => ComparisonOp::Eq,
                BinaryOpKind::GreaterThan | BinaryOpKind::GreaterOrEqual => ComparisonOp::Ge,
                BinaryOpKind::LessThan | BinaryOpKind::LessOrEqual => ComparisonOp::Le,
                _ => return None,
            };

            let mut extractor = LinearConstraintExtractor {
                terms: HashMap::new(),
                constant: 0,
                comparison,
            };

            extractor.collect_terms(lhs, 1)?;
            extractor.collect_terms(rhs, -1)?;

            Some(extractor.into_constraint())
        } else {
            None
        }
    }

    fn collect_terms(&mut self, expr: &Expr, sign: i32) -> Option<()> {
        match &expr.kind {
            ExprKind::Number(n) => self.constant += sign * n,
            ExprKind::OwnedIdent(ident) => {
                // A variable with no explicit coefficient (which means the coefficient is 1)
                if let Identifier::Resolved { owner, name } = ident.as_ref() {
                    let coefficient = self.terms.entry(owner.symbol_id(name)).or_default();
                    *coefficient += sign;
                } else {
                    panic!("Unresolved identifier")
                }
            }
            ExprKind::UnaryOp(UnaryOpKind::Negation, expr) => self.collect_terms(expr, -sign)?, // Flip sign
            ExprKind::BinaryOp(operator, lhs, rhs) => match operator {
                BinaryOpKind::Addition => {
                    self.collect_terms(rhs, sign)?;
                    self.collect_terms(lhs, sign)?;
                }
                BinaryOpKind::Subtraction => {
                    self.collect_terms(rhs, sign)?;
                    self.collect_terms(lhs, -sign)?; // Note: flipped sign
                }
                BinaryOpKind::Multiplication => {
                    // One side must be a constant, the other must be a variable
                    if let (ExprKind::Number(n), ExprKind::OwnedIdent(ident))
                    | (ExprKind::OwnedIdent(ident), ExprKind::Number(n)) = (&lhs.kind, &rhs.kind)
                    {
                        if let Identifier::Resolved { owner, name } = ident.as_ref() {
                            let coefficient = self.terms.entry(owner.symbol_id(name)).or_default();
                            *coefficient += n * sign;
                        } else {
                            panic!("Unresolved identifier")
                        }
                    }
                    return None;
                }
                BinaryOpKind::Division => {
                    // Variable divided with a constant is a linear expression,
                    // but constant divided by variable is not.
                    if let (ExprKind::OwnedIdent(ident), ExprKind::Number(n)) =
                        (&lhs.kind, &rhs.kind)
                    {
                        if let Identifier::Resolved { owner, name } = ident.as_ref() {
                            let coefficient = self.terms.entry(owner.symbol_id(name)).or_default();
                            *coefficient += (1 / n) * sign;
                        } else {
                            panic!("Unresolved identifier")
                        }
                    }
                    return None;
                }
                _ => return None,
            },
            _ => return None,
        }
        Some(())
    }

    fn into_constraint(mut self) -> LinearConstraint {
        let terms: Vec<(SymbolIdentifier, i32)> = self.terms.drain().collect();
        let coefficient_norm = terms
            .iter()
            .map(|(_, coefficient)| (coefficient * coefficient) as f64)
            .sum::<f64>()
            .sqrt();

        LinearConstraint {
            terms,
            constant: self.constant,
            comparison: self.comparison,
            coefficient_norm,
        }
    }
}
