use crate::algorithms::certain_zero::search_strategy::linear_constraints::{
    ComparisonOp, LinearConstraint, LinearConstraintExtractor,
};
use crate::atl::Phi;
use crate::game_structure::lcgs::ast::{
    BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind,
};
use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::Proposition;
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A structure describing the relation between linear constraints in a ATL formula
#[derive(Clone)]
pub enum LinearConstrainedPhi {
    /// Either or should hold
    Or(Box<LinearConstrainedPhi>, Box<LinearConstrainedPhi>),
    /// Both should hold
    And(Box<LinearConstrainedPhi>, Box<LinearConstrainedPhi>),
    /// Mapping symbols to ranges
    Constraint(LinearConstraint),
    True,
    False,
}

pub struct ConstrainedPhiMapper {
    /// Cached LinearConstrainedPhi of propositions
    proposition_cache: RefCell<HashMap<Proposition, LinearConstrainedPhi>>,
}

impl ConstrainedPhiMapper {
    pub fn new() -> ConstrainedPhiMapper {
        ConstrainedPhiMapper {
            proposition_cache: RefCell::new(HashMap::new()),
        }
    }

    /// Map the given phi to a linear constrained phi. Propositions will be map based on the given
    /// game and they will be cached, so the same game should be used every time.
    pub fn map(&self, game: &IntermediateLcgs, phi: &Phi) -> LinearConstrainedPhi {
        self.map_phi_to_constraints(game, phi)
    }

    /// Takes a phi and maps it to a LinearConstrainedPhi
    fn map_phi_to_constraints(&self, game: &IntermediateLcgs, phi: &Phi) -> LinearConstrainedPhi {
        match phi {
            Phi::True => LinearConstrainedPhi::True,
            Phi::False => LinearConstrainedPhi::False,
            Phi::Proposition(proposition) => {
                // If we get to a proposition, continue the mapping inside the proposition's condition
                // The result may be cached
                let maybe_constraint = self.proposition_cache.borrow().get(proposition).cloned();
                maybe_constraint.unwrap_or_else(|| {
                    // We have not mapped this proposition yet
                    let decl = game.label_index_to_decl(*proposition);
                    if let DeclKind::Label(label) = &decl.kind {
                        let mapped_expr = self.map_expr_to_constraints(&label.condition);
                        // Save result in cache
                        self.proposition_cache
                            .borrow_mut()
                            .insert(*proposition, mapped_expr.clone());
                        mapped_expr
                    } else {
                        panic!("Non-propositions symbol in ATL formula")
                    }
                })
            }
            Phi::Not(formula) => self.map_phi_to_constraints(game, formula),
            Phi::Or(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_constraints(game, lhs);
                let rhs_symbol_range_map = self.map_phi_to_constraints(game, rhs);

                LinearConstrainedPhi::Or(
                    Box::from(lhs_symbol_range_map),
                    Box::from(rhs_symbol_range_map),
                )
            }
            Phi::And(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_constraints(game, lhs);
                let rhs_symbol_range_map = self.map_phi_to_constraints(game, rhs);

                LinearConstrainedPhi::And(
                    Box::from(lhs_symbol_range_map),
                    Box::from(rhs_symbol_range_map),
                )
            }
            Phi::DespiteNext { formula, .. } => self.map_phi_to_constraints(game, formula),
            Phi::EnforceNext { formula, .. } => self.map_phi_to_constraints(game, formula),
            Phi::DespiteUntil { pre, until, .. } => {
                let pre_symbol_range_map = self.map_phi_to_constraints(game, pre);
                let until_symbol_range_map = self.map_phi_to_constraints(game, until);

                LinearConstrainedPhi::Or(
                    Box::from(pre_symbol_range_map),
                    Box::from(until_symbol_range_map),
                )
            }
            Phi::EnforceUntil { pre, until, .. } => {
                let pre_symbol_range_map = self.map_phi_to_constraints(game, pre);
                let until_symbol_range_map = self.map_phi_to_constraints(game, until);

                LinearConstrainedPhi::Or(
                    Box::from(pre_symbol_range_map),
                    Box::from(until_symbol_range_map),
                )
            }
            Phi::DespiteEventually { formula, .. } => self.map_phi_to_constraints(game, formula),
            Phi::EnforceEventually { formula, .. } => self.map_phi_to_constraints(game, formula),
            Phi::DespiteInvariant { formula, .. } => self.map_phi_to_constraints(game, formula),
            Phi::EnforceInvariant { formula, .. } => self.map_phi_to_constraints(game, formula),
        }
    }

    /// Takes an expression and maps it to a LinearConstrainedPhi
    fn map_expr_to_constraints(&self, expr: &Expr) -> LinearConstrainedPhi {
        match &expr.kind {
            ExprKind::UnaryOp(UnaryOpKind::Not, sub_expr) => {
                return self.map_expr_to_constraints(sub_expr);
            }
            ExprKind::BinaryOp(operator, lhs, rhs) => {
                match operator {
                    BinaryOpKind::Equality
                    | BinaryOpKind::GreaterThan
                    | BinaryOpKind::GreaterOrEqual
                    | BinaryOpKind::LessThan
                    | BinaryOpKind::LessOrEqual => {
                        let lin_expr = LinearConstraintExtractor::extract(expr);
                        return if let Some(lin_expr) = lin_expr {
                            LinearConstrainedPhi::Constraint(lin_expr)
                        } else {
                            // Not linear
                            LinearConstrainedPhi::True
                        };
                    }
                    BinaryOpKind::And => {
                        let lhs_con = self.map_expr_to_constraints(lhs);
                        let rhs_con = self.map_expr_to_constraints(rhs);
                        return LinearConstrainedPhi::And(Box::new(lhs_con), Box::new(rhs_con));
                    }
                    BinaryOpKind::Or => {
                        let lhs_con = self.map_expr_to_constraints(lhs);
                        let rhs_con = self.map_expr_to_constraints(rhs);
                        return LinearConstrainedPhi::Or(Box::new(lhs_con), Box::new(rhs_con));
                    }
                    // P -> Q == not P v Q, we don't care about not, so becomes P v Q
                    BinaryOpKind::Implication => {
                        let lhs_con = self.map_expr_to_constraints(lhs);
                        let rhs_con = self.map_expr_to_constraints(rhs);
                        return LinearConstrainedPhi::Or(Box::new(lhs_con), Box::new(rhs_con));
                    }
                    _ => {}
                }
            }
            ExprKind::Number(n) => {
                return if *n == 0 {
                    LinearConstrainedPhi::False
                } else {
                    LinearConstrainedPhi::True
                }
            }
            // https://en.wikipedia.org/wiki/Conditioned_disjunction
            // Q ? P : R == (Q -> P) and (not Q -> R) == (Q and P) or (not Q and R)
            // we don't care about not, so becomes (Q and P) or (Q and R), and can be written as
            // (Q and (P or R))
            ExprKind::TernaryIf(q, p, r) => {
                let q = self.map_expr_to_constraints(q);
                let p = self.map_expr_to_constraints(p);
                let r = self.map_expr_to_constraints(r);
                return LinearConstrainedPhi::And(
                    Box::new(q),
                    Box::from(LinearConstrainedPhi::Or(Box::new(p), Box::new(r))),
                );
            }
            // Some other expression can be converted to a comparisons since everything != 0 is true
            ExprKind::OwnedIdent(ident) => {
                let mut terms_hashmap = HashMap::new();
                if let Identifier::Resolved { owner, name } = ident.as_ref() {
                    terms_hashmap.insert(owner.symbol_id(name), 1.0);
                }

                let less_constraint = LinearConstraint {
                    terms: terms_hashmap.clone(),
                    constant: 0.0,
                    comparison: ComparisonOp::Less,
                    coefficient_norm: 1.0,
                };

                let greater_constraint = LinearConstraint {
                    terms: terms_hashmap,
                    constant: 0.0,
                    comparison: ComparisonOp::Greater,
                    coefficient_norm: 1.0,
                };

                return LinearConstrainedPhi::Or(
                    Box::new(LinearConstrainedPhi::Constraint(less_constraint)),
                    Box::new(LinearConstrainedPhi::Constraint(greater_constraint)),
                );
            }

            _ => {}
        }

        // Not linear
        LinearConstrainedPhi::True
    }
}
