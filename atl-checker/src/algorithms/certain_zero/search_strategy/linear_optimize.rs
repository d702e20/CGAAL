use crate::algorithms::certain_zero::search_strategy::linear_constraints::{
    LinearConstraint, LinearConstraintExtractor,
};
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::Phi;
use crate::edg::atlcgsedg::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, UnaryOpKind};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::game_structure::Proposition;
use crate::game_structure::State as StateUsize;
use float_ord::FloatOrd;
use priority_queue::PriorityQueue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::option::Option::Some;
use std::sync::Arc;

/// A SearchStrategyBuilder for building the LinearOptimizeSearch strategy.
pub struct LinearOptimizeSearchBuilder {
    pub game: IntermediateLcgs,
}

impl SearchStrategyBuilder<AtlVertex, LinearOptimizeSearch> for LinearOptimizeSearchBuilder {
    fn build(&self) -> LinearOptimizeSearch {
        LinearOptimizeSearch::new(self.game.clone())
    }
}

/// Search strategy using ideas from linear programming to order next vertices,
/// based on distance from the vertex to a region that borders the line between true/false in the formula.
pub struct LinearOptimizeSearch {
    /// Priority based on distance, lowest distance highest priority
    queue: PriorityQueue<Edge<AtlVertex>, FloatOrd<f64>>,
    game: IntermediateLcgs,
    /// Maps the hash of a Phi and usize of state, to a result distance
    result_cache: HashMap<(Arc<Phi>, StateUsize), FloatOrd<f64>>,
    /// Maps proposition to the linear constraint the are
    proposition_cache: RefCell<HashMap<Proposition, LinearConstrainedPhi>>,
    /// Maps phi to a LinearConstrainedPhi
    phi_cache: HashMap<Arc<Phi>, LinearConstrainedPhi>,
    // TODO - could add a new cache, to compare distance between states and use this to guesstimate distance
    // TODO - to acceptance region, based on previous results (Mathias supervisor suggestion)
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLcgs) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: PriorityQueue::new(),
            game,
            result_cache: HashMap::new(),
            proposition_cache: RefCell::new(HashMap::new()),
            phi_cache: HashMap::new(),
        }
    }
}

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

impl SearchStrategy<AtlVertex> for LinearOptimizeSearch {
    /// Simply returns the edge with highest priority (i.e lowest distance)
    fn next(&mut self) -> Option<Edge<AtlVertex>> {
        self.queue.pop().map(|entry| entry.0)
    }

    /// Takes a Vec of Edges holding ATLVertices, and puts these in the queue,
    /// based on the calculated distance from its state to acceptance region from formula
    fn queue_new_edges(&mut self, edges: Vec<Edge<AtlVertex>>) {
        for edge in edges {
            let distance = self.get_distance_in_atl_vertex(edge.source());

            // Add edge and distance to queue
            if let Some(dist) = distance {
                self.queue.push(edge, FloatOrd(-dist.0));
            } else {
                // Todo what should default value be, if cannot be calculated? For now: first priority
                self.queue.push(edge, FloatOrd(0.0));
            }
        }
    }
}

impl LinearOptimizeSearch {
    /// Finds the distance in a single atl_vertex
    fn get_distance_in_atl_vertex(&mut self, target: &AtlVertex) -> Option<FloatOrd<f64>> {
        // If we have seen this phi before, and this state, get the result instantly
        if let Some(distance) = self.result_cache.get(&(target.formula(), target.state())) {
            return Some(*distance);
        }

        // If we have not seen this formula before
        if !self.phi_cache.contains_key(&target.formula()) {
            // Convert the formula to a structure of constraints
            self.phi_cache.insert(
                target.formula(),
                self.map_phi_to_constraints(&*target.formula()),
            );
        }

        // Get constraints of this phi
        let constrained_phi = self.phi_cache.get(&target.formula()).unwrap();
        // Get current state in vertex
        let state = self.game.state_from_index(target.state());

        // Find the distance to constraints by visiting the constrained phi
        if let Some(distance) = self.visit_constrained_phi(constrained_phi, &state) {
            // Cache the resulting distance from the combination of this formula and state
            self.result_cache
                .insert((target.formula(), target.state()), distance);
            // Return calculated distance
            return Some(distance);
        }
        // If we could not find a distance, return None
        None
    }

    /// Takes a phi and maps it to a LinearConstrainedPhi
    fn map_phi_to_constraints(&self, phi: &Phi) -> LinearConstrainedPhi {
        match phi {
            Phi::True => LinearConstrainedPhi::True,
            Phi::False => LinearConstrainedPhi::False,
            Phi::Proposition(proposition) => {
                // If we get to a proposition, continue the mapping inside the proposition's condition
                // The result may be cached
                let maybe_constraint = self.proposition_cache.borrow().get(proposition).cloned();
                maybe_constraint.unwrap_or_else(|| {
                    // We have not mapped this proposition yet
                    let decl = self.game.label_index_to_decl(*proposition);
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
            Phi::Not(formula) => self.map_phi_to_constraints(formula),
            Phi::Or(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_constraints(lhs);
                let rhs_symbol_range_map = self.map_phi_to_constraints(rhs);

                LinearConstrainedPhi::Or(
                    Box::from(lhs_symbol_range_map),
                    Box::from(rhs_symbol_range_map),
                )
            }
            Phi::And(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_constraints(lhs);
                let rhs_symbol_range_map = self.map_phi_to_constraints(rhs);

                LinearConstrainedPhi::And(
                    Box::from(lhs_symbol_range_map),
                    Box::from(rhs_symbol_range_map),
                )
            }
            Phi::DespiteNext { formula, .. } => self.map_phi_to_constraints(formula),
            Phi::EnforceNext { formula, .. } => self.map_phi_to_constraints(formula),
            Phi::DespiteUntil { pre, until, .. } => {
                let pre_symbol_range_map = self.map_phi_to_constraints(pre);
                let until_symbol_range_map = self.map_phi_to_constraints(until);

                LinearConstrainedPhi::Or(
                    Box::from(pre_symbol_range_map),
                    Box::from(until_symbol_range_map),
                )
            }
            Phi::EnforceUntil { pre, until, .. } => {
                let pre_symbol_range_map = self.map_phi_to_constraints(pre);
                let until_symbol_range_map = self.map_phi_to_constraints(until);

                LinearConstrainedPhi::Or(
                    Box::from(pre_symbol_range_map),
                    Box::from(until_symbol_range_map),
                )
            }
            Phi::DespiteEventually { formula, .. } => self.map_phi_to_constraints(formula),
            Phi::EnforceEventually { formula, .. } => self.map_phi_to_constraints(formula),
            Phi::DespiteInvariant { formula, .. } => self.map_phi_to_constraints(formula),
            Phi::EnforceInvariant { formula, .. } => self.map_phi_to_constraints(formula),
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
                    _ => {}
                }
            }
            // TODO Some other expression can be converted to a comparisons since everything != 0 is true
            _ => {}
        }

        // Not linear
        LinearConstrainedPhi::True
    }

    /// Goes through the LinearConstrainedPhi and finds how close we are to acceptance border in this state.
    /// A return value of `None` represents an undefined distance.
    fn visit_constrained_phi(
        &self,
        ranged_phi: &LinearConstrainedPhi,
        state: &State,
    ) -> Option<FloatOrd<f64>> {
        match ranged_phi {
            LinearConstrainedPhi::Or(lhs, rhs) => {
                // If we need to satisfy either of the formulas, just return the lowest distance found between the two
                // If one the the sides is None, the other is returned (given that the other is Some)
                // This makes sure that "x < 5 || false" will not return None, but the dist in lhs
                let lhs_dist = self.visit_constrained_phi(lhs, state);
                let rhs_dist = self.visit_constrained_phi(rhs, state);
                match (lhs_dist, rhs_dist) {
                    (Some(l), Some(r)) => Some(Ord::min(l, r)),
                    (Some(l), None) => Some(l),
                    (None, Some(r)) => Some(r),
                    _ => None,
                }
            }
            LinearConstrainedPhi::And(lhs, rhs) => {
                // If we need to satisfy both of the formulas, just return the largest distance found between the two
                Ord::max(
                    self.visit_constrained_phi(lhs, state),
                    self.visit_constrained_phi(rhs, state),
                )
            }
            LinearConstrainedPhi::Constraint(constraint) => {
                // Find distance to constraint
                Some(distance_to_constraint(constraint, state))
            }
            LinearConstrainedPhi::True => Some(FloatOrd(0.0)),
            LinearConstrainedPhi::False => None,
        }
    }
}

/// Returns the distance between the given state and the constraint.
fn distance_to_constraint(constraint: &LinearConstraint, state: &State) -> FloatOrd<f64> {
    // This is essentially the distance between a point and a line
    // https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line

    let mut numerator = constraint.constant;
    for (symbol, coefficient) in &constraint.terms {
        let v = state.0.get(symbol).unwrap();
        numerator += coefficient * (*v as f64);
    }

    FloatOrd(numerator.abs() as f64 / constraint.coefficient_norm)
}
