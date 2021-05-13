use crate::algorithms::certain_zero::search_strategy::linear_constraints::{
    LinearConstraint, LinearConstraintExtractor,
};
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::Phi;
use crate::edg::atlcgsedg::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::ast::DeclKind;
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::game_structure::Proposition;
use crate::game_structure::State as StateUsize;
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
    queue: PriorityQueue<Edge<AtlVertex>, i32>,
    game: IntermediateLcgs,
    /// Maps the hash of a Phi and usize of state, to a result distance
    result_cache: HashMap<(Arc<Phi>, StateUsize), i32>,
    /// Maps proposition to the linear constraint the are
    proposition_cache: RefCell<HashMap<Proposition, Option<LinearConstraint>>>,
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

/// Used when mapping a Phi to a LinearConstrainedPhi variant,
/// where the propositions have been replaced by hashmaps mapping symbols to Ranges
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
            let distance = self.get_distance_in_edge(&edge);

            // Add edge and distance to queue
            if let Some(dist) = distance {
                self.queue.push(edge, -dist);
            } else {
                // Todo what should default value be, if cannot be calculated? For now: first priority
                self.queue.push(edge, 0);
            }
        }
    }
}

impl LinearOptimizeSearch {
    /// if edge is a HyperEdge, return average distance from state to accept region between all targets,
    /// if Negation edge, just return the distance from its target
    fn get_distance_in_edge(&mut self, edge: &Edge<AtlVertex>) -> Option<i32> {
        match &edge {
            Edge::Hyper(hyperedge) => {
                // For every target of the hyperedge, we want to see how close we are to acceptance border
                let mut distances: Vec<f32> = Vec::new();
                for target in &hyperedge.targets {
                    if let Some(dist) = self.get_distance_in_atl_vertex(target) {
                        distances.push(dist as f32)
                    }
                }

                // If no targets were able to satisfy formula, or something went wrong, return None
                return if distances.is_empty() {
                    None
                } else {
                    // Find average distance between targets, and return this
                    let avg_distance = distances.iter().sum::<f32>() / distances.len() as f32;
                    Some(avg_distance as i32)
                };
            }
            // Same procedure for negation edges as for hyper, just no for loop for all targets, as we only have one target
            Edge::Negation(edge) => self.get_distance_in_atl_vertex(&edge.target),
        }
    }

    /// Finds the distance in a single atl_vertex
    fn get_distance_in_atl_vertex(&mut self, target: &AtlVertex) -> Option<i32> {
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
            let distance = distance as i32;
            // Cache the resulting distance from the combination of this formula and state
            self.result_cache
                .insert((target.formula(), target.state()), distance);
            // Return calculated distance
            return Some(distance);
        }
        // If we could not find a distance, return None
        None
    }

    /// Takes a Phi (the formula in the vertex) and maps it to a LinearConstrainedPhi, using the Ranges that we computed for the propositions
    fn map_phi_to_constraints(&self, phi: &Phi) -> LinearConstrainedPhi {
        match phi {
            Phi::True => LinearConstrainedPhi::True,
            Phi::False => LinearConstrainedPhi::False,
            Phi::Proposition(proposition) => {
                // If we get to a proposition, find the Range associated with it, in the proposition_cache and return this
                let maybe_constraint = self.proposition_cache.borrow().get(proposition).cloned();
                let constraint = maybe_constraint.unwrap_or_else(|| {
                    // We have not tried to extract the linear expression from this proposition yet. Let's try
                    let decl = self.game.label_index_to_decl(*proposition);
                    if let DeclKind::Label(label) = &decl.kind {
                        let lin_expr = LinearConstraintExtractor::extract(&label.condition);
                        // Save result in cache
                        self.proposition_cache
                            .borrow_mut()
                            .insert(*proposition, lin_expr.clone());
                        lin_expr
                    } else {
                        None
                    }
                });
                // Convert to LinearConstrainedPhi::Contraint if it is a linear expression, or true otherwise
                constraint
                    .map(|c| LinearConstrainedPhi::Constraint(c))
                    .or(Some(LinearConstrainedPhi::True))
                    .unwrap()
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

    /// Goes through the RangedPhi and finds how close we are to acceptance border in this state.
    /// A return value of `None` represents an undefined distance.
    fn visit_constrained_phi(
        &self,
        ranged_phi: &LinearConstrainedPhi,
        state: &State,
    ) -> Option<i32> {
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
            LinearConstrainedPhi::True => Some(0),
            LinearConstrainedPhi::False => None,
        }
    }
}

/// Returns the distance between the given state and the constraint.
fn distance_to_constraint(constraint: &LinearConstraint, state: &State) -> i32 {
    // This is essentially the distance between a point and a line
    // https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line

    let mut numerator = constraint.constant;
    for (symbol, coefficient) in &constraint.terms {
        let v = state.0.get(symbol).unwrap();
        numerator += coefficient * v;
    }

    (numerator.abs() as f64 / constraint.coefficient_norm) as i32
}
