use crate::algorithms::certain_zero::search_strategy::linear_constraints::{
    ComparisonOp, LinearConstraint, LinearConstraintExtractor,
};
use crate::atl::Phi;
use crate::game_structure::lcgs::ast::{
    BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind,
};
use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
use crate::game_structure::Proposition;
use std::cell::RefCell;
use std::collections::HashMap;

/// A structure describing the relation between linear constraints in a ATL formula
#[derive(Clone)]
pub enum LinearConstrainedPhi {
    /// Either or should hold
    Or(Box<LinearConstrainedPhi>, Box<LinearConstrainedPhi>),
    /// Both should hold
    And(Box<LinearConstrainedPhi>, Box<LinearConstrainedPhi>),
    /// Mapping symbols to ranges
    Constraint(LinearConstraint),
    /// Proposition that is not linear. We can't use these in the linear methods, but it is not
    /// definitively true or false either
    NonLinear,
    True,
    False,
}

impl LinearConstrainedPhi {
    /// Returns the negations of this constrained phi
    pub fn negated(&self) -> Self {
        match self {
            LinearConstrainedPhi::Or(lhs, rhs) => {
                LinearConstrainedPhi::And(Box::new(lhs.negated()), Box::new(rhs.negated()))
            }
            LinearConstrainedPhi::And(lhs, rhs) => {
                LinearConstrainedPhi::Or(Box::new(lhs.negated()), Box::new(rhs.negated()))
            }
            LinearConstrainedPhi::Constraint(constraint) => {
                LinearConstrainedPhi::Constraint(constraint.negated())
            }
            LinearConstrainedPhi::True => LinearConstrainedPhi::False,
            LinearConstrainedPhi::False => LinearConstrainedPhi::True,
            LinearConstrainedPhi::NonLinear => LinearConstrainedPhi::NonLinear,
        }
    }
}

pub struct ConstrainedPhiMaker {
    /// Cached LinearConstrainedPhi of propositions. The bool is true for negated propositions.
    proposition_cache: RefCell<HashMap<(Proposition, bool), LinearConstrainedPhi>>,
}

impl ConstrainedPhiMaker {
    pub fn new() -> ConstrainedPhiMaker {
        ConstrainedPhiMaker {
            proposition_cache: RefCell::new(HashMap::new()),
        }
    }

    /// Map the given phi to a linear constrained phi. Propositions will be mapped based on the
    /// given game and they will be cached, so the same game should be used every time.
    pub fn convert(&self, game: &IntermediateLcgs, phi: &Phi) -> LinearConstrainedPhi {
        self.map_phi_to_constraints(game, phi, false)
    }

    /// Takes a phi and maps it to a LinearConstrainedPhi
    fn map_phi_to_constraints(
        &self,
        game: &IntermediateLcgs,
        phi: &Phi,
        negated: bool,
    ) -> LinearConstrainedPhi {
        match phi {
            Phi::True => {
                if !negated {
                    LinearConstrainedPhi::True
                } else {
                    LinearConstrainedPhi::False
                }
            }
            Phi::False => {
                if !negated {
                    LinearConstrainedPhi::False
                } else {
                    LinearConstrainedPhi::True
                }
            }
            Phi::Proposition(proposition) => {
                // If we get to a proposition, continue the mapping inside the proposition's condition
                // The result may be cached
                let key = (*proposition, negated);
                let maybe_constraint = self.proposition_cache.borrow().get(&key).cloned();
                maybe_constraint.unwrap_or_else(|| {
                    // We have not mapped this proposition yet
                    let decl = game.label_index_to_decl(*proposition);
                    if let DeclKind::Label(label) = &decl.kind {
                        let mapped_expr = Self::map_expr_to_constraints(&label.condition, negated);
                        // Save result in cache
                        self.proposition_cache
                            .borrow_mut()
                            .insert(key, mapped_expr.clone());
                        mapped_expr
                    } else {
                        panic!("Non-propositions symbol in ATL formula")
                    }
                })
            }
            Phi::Not(formula) => self.map_phi_to_constraints(game, formula, !negated),
            Phi::Or(lhs, rhs) => {
                let lhs_lcp = self.map_phi_to_constraints(game, lhs, negated);
                let rhs_lcp = self.map_phi_to_constraints(game, rhs, negated);
                if !negated {
                    LinearConstrainedPhi::Or(Box::new(lhs_lcp), Box::new(rhs_lcp))
                } else {
                    LinearConstrainedPhi::And(Box::new(lhs_lcp), Box::new(rhs_lcp))
                }
            }
            Phi::And(lhs, rhs) => {
                let lhs_lcp = self.map_phi_to_constraints(game, lhs, negated);
                let rhs_lcp = self.map_phi_to_constraints(game, rhs, negated);
                if !negated {
                    LinearConstrainedPhi::And(Box::new(lhs_lcp), Box::new(rhs_lcp))
                } else {
                    LinearConstrainedPhi::Or(Box::new(lhs_lcp), Box::new(rhs_lcp))
                }
            }
            Phi::DespiteNext { formula, .. } => self.map_phi_to_constraints(game, formula, negated),
            Phi::EnforceNext { formula, .. } => self.map_phi_to_constraints(game, formula, negated),
            Phi::DespiteUntil { pre, until, .. } => {
                let pre_lcp = self.map_phi_to_constraints(game, pre, negated);
                let until_lcp = self.map_phi_to_constraints(game, until, negated);
                if !negated {
                    LinearConstrainedPhi::Or(Box::new(pre_lcp), Box::new(until_lcp))
                } else {
                    LinearConstrainedPhi::And(Box::new(pre_lcp), Box::new(until_lcp))
                }
            }
            Phi::EnforceUntil { pre, until, .. } => {
                let pre_lcp = self.map_phi_to_constraints(game, pre, negated);
                let until_lcp = self.map_phi_to_constraints(game, until, negated);
                if !negated {
                    LinearConstrainedPhi::Or(Box::new(pre_lcp), Box::new(until_lcp))
                } else {
                    LinearConstrainedPhi::And(Box::new(pre_lcp), Box::new(until_lcp))
                }
            }
            Phi::DespiteEventually { formula, .. } => {
                self.map_phi_to_constraints(game, formula, negated)
            }
            Phi::EnforceEventually { formula, .. } => {
                self.map_phi_to_constraints(game, formula, negated)
            }
            Phi::DespiteInvariant { formula, .. } => {
                self.map_phi_to_constraints(game, formula, negated)
            }
            Phi::EnforceInvariant { formula, .. } => {
                self.map_phi_to_constraints(game, formula, negated)
            }
        }
    }

    /// Takes an expression and maps it to a LinearConstrainedPhi
    fn map_expr_to_constraints(expr: &Expr, negated: bool) -> LinearConstrainedPhi {
        match &expr.kind {
            ExprKind::UnaryOp(UnaryOpKind::Not, sub_expr) => {
                return Self::map_expr_to_constraints(sub_expr, !negated);
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
                            if !negated {
                                LinearConstrainedPhi::Constraint(lin_expr)
                            } else {
                                LinearConstrainedPhi::Constraint(lin_expr.negated())
                            }
                        } else {
                            LinearConstrainedPhi::NonLinear
                        };
                    }
                    BinaryOpKind::And => {
                        let lhs_con = Self::map_expr_to_constraints(lhs, negated);
                        let rhs_con = Self::map_expr_to_constraints(rhs, negated);
                        return if !negated {
                            LinearConstrainedPhi::And(Box::new(lhs_con), Box::new(rhs_con))
                        } else {
                            LinearConstrainedPhi::Or(Box::new(lhs_con), Box::new(rhs_con))
                        };
                    }
                    BinaryOpKind::Or => {
                        let lhs_con = Self::map_expr_to_constraints(lhs, negated);
                        let rhs_con = Self::map_expr_to_constraints(rhs, negated);
                        return if !negated {
                            LinearConstrainedPhi::Or(Box::new(lhs_con), Box::new(rhs_con))
                        } else {
                            LinearConstrainedPhi::And(Box::new(lhs_con), Box::new(rhs_con))
                        };
                    }
                    // P -> Q == not P v Q
                    BinaryOpKind::Implication => {
                        let lhs_con = Self::map_expr_to_constraints(lhs, !negated);
                        let rhs_con = Self::map_expr_to_constraints(rhs, negated);
                        return if !negated {
                            LinearConstrainedPhi::Or(Box::new(lhs_con), Box::new(rhs_con))
                        } else {
                            LinearConstrainedPhi::And(Box::new(lhs_con), Box::new(rhs_con))
                        };
                    }
                    _ => {}
                }
            }
            ExprKind::Number(n) => {
                // Think of != as XOR in this situation
                return if (*n == 0) != negated {
                    LinearConstrainedPhi::False
                } else {
                    LinearConstrainedPhi::True
                };
            }
            // https://en.wikipedia.org/wiki/Conditioned_disjunction
            // Q ? P : R == (Q -> P) and (not Q -> R) == (Q and P) or (not Q and R)
            ExprKind::TernaryIf(q, p, r) => {
                let q = Self::map_expr_to_constraints(q, negated);
                let not_q = q.negated();
                let p = Self::map_expr_to_constraints(p, negated);
                let r = Self::map_expr_to_constraints(r, negated);
                return if !negated {
                    LinearConstrainedPhi::Or(
                        Box::new(LinearConstrainedPhi::And(Box::new(q), Box::new(p))),
                        Box::new(LinearConstrainedPhi::And(Box::new(not_q), Box::new(r))),
                    )
                } else {
                    // If negated, then we want to produce not (Q and P) or (not Q and R)
                    // == (not (Q and P)) and (not (not Q and R))
                    // == (not Q or not P) and (Q or not R)
                    // And all terms are negated since negated was passed to it, so ultimately
                    // we get (Q or P) and (not Q or R)
                    LinearConstrainedPhi::And(
                        Box::new(LinearConstrainedPhi::Or(Box::new(q), Box::new(p))),
                        Box::new(LinearConstrainedPhi::Or(Box::new(not_q), Box::new(r))),
                    )
                };
            }
            // This is essentially x != 0, if x is the name of the symbol
            ExprKind::OwnedIdent(ident) => {
                let mut terms_hashmap = HashMap::new();
                if let Identifier::Resolved { owner, name } = ident.as_ref() {
                    terms_hashmap.insert(owner.symbol_id(name), 1.0);
                }

                let operator = if !negated {
                    ComparisonOp::NotEqual
                } else {
                    ComparisonOp::Equal
                };

                let constraint = LinearConstraint {
                    terms: terms_hashmap,
                    constant: 0.0,
                    comparison: operator,
                    coefficient_norm: 1.0,
                };

                return LinearConstrainedPhi::Constraint(constraint);
            }
            // TODO Other expression can be converted to a comparisons since everything != 0 is true
            _ => {}
        }

        // Not linear
        LinearConstrainedPhi::NonLinear
    }
}
