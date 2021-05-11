use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::Phi;
use crate::edg::atlcgsedg::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, ExprKind, Identifier};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::Proposition;
use crate::game_structure::State as StateUsize;
use minilp::OptimizationDirection::{Maximize, Minimize};
use minilp::{ComparisonOp, OptimizationDirection, Problem};
use priority_queue::PriorityQueue;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::ops::Range;
use std::option::Option::Some;
use std::sync::Arc;
use BinaryOpKind::{Equality, GreaterThan, LessThan};

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

/// Holds extracted linear expressions from the formula in AtlVertex
/// E.g. C = ax + by + cz
#[derive(Clone)]
pub struct LinearConstraint {
    pub terms: Vec<(i32, SymbolIdentifier)>,
    pub constant: i32,
    pub operation: ComparisonOp,
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
                // Todo what should default value be, if cannot be calculated?
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
            Edge::Negation(edge) => {
                if let Some(distance) = self.get_distance_in_atl_vertex(&edge.target) {
                    return Some(distance);
                }
                None
            }
        }
    }

    /// Finds the distance in a single atl_vertex
    fn get_distance_in_atl_vertex(&mut self, target: &AtlVertex) -> Option<i32> {
        // If we have seen this phi before, and this state, get the result instantly
        if let Some(distance) = self.result_cache.get(&(target.formula(), target.state())) {
            return Some(*distance);
        } else {
            // TODO could check if current state is close to something in the cache, and just reuse that result,
            // TODO or perhaps look for more results close to this state, and extrapolate (Mathias supervisor suggestion)
        }

        // If we have not seen this formula before
        if !self.phi_cache.contains_key(&target.formula()) {
            // Now we know the ranges for which the propositions in the formula would be satisfied,
            // Map the phi to one that holds Ranges, and cache this
            self.phi_cache.insert(
                target.formula(),
                self.map_phi_to_constraints(&*target.formula()),
            );
        }

        // Now we have the mapped phi (called RangedPhi)
        if let Some(ranged_phi) = self.phi_cache.get(&target.formula()) {
            // Get current state in vertex
            let state = self.game.state_from_index(target.state());
            // Find the distance to acceptance region by visting the RangedPhi representing the formula in the vertex
            if let Some(distance) = self.visit_ranged_phi(ranged_phi, &state) {
                // Cache the resulting distance from the combination of this formula and state
                self.result_cache
                    .insert((target.formula(), target.state()), distance);
                // Return calculated distance
                return Some(distance);
            }
        }
        // If something went wrong, and we could not find a distance, return None
        None
    }

    // TODO should be replaced by something better
    /// Finds a single simple linear expression
    fn extract_outer_comparison(&self, expr: &ExprKind) -> Option<LinearConstraint> {
        match &expr {
            ExprKind::BinaryOp(operator, lhs, rhs) => {
                // Either we have that lhs is the constant, and rhs is the constraints
                if let ExprKind::OwnedIdent(id) = &lhs.kind {
                    if let Identifier::Resolved { owner, name } = *id.clone() {
                        let symbol_of_id = SymbolIdentifier {
                            owner,
                            name: name.parse().unwrap(),
                        };
                        if let ExprKind::Number(number) = rhs.kind {
                            return Some(LinearConstraint {
                                terms: vec![(1, symbol_of_id)],
                                constant: number,
                                operation: match operator {
                                    Equality => ComparisonOp::Eq,
                                    GreaterThan => ComparisonOp::Ge,
                                    LessThan => ComparisonOp::Le,
                                    _ => {
                                        return None;
                                    }
                                },
                            });
                        }
                    }
                    // Or we have that rhs is the constant, and lhs is the constraints
                } else if let ExprKind::OwnedIdent(id) = &rhs.kind {
                    if let Identifier::Resolved { owner, name } = *id.clone() {
                        let symbol_of_id = SymbolIdentifier {
                            owner,
                            name: name.parse().unwrap(),
                        };
                        if let ExprKind::Number(number) = lhs.kind {
                            return Some(LinearConstraint {
                                terms: vec![(1, symbol_of_id)],
                                constant: number,
                                operation: match operator {
                                    Equality => ComparisonOp::Eq,
                                    GreaterThan => ComparisonOp::Ge,
                                    LessThan => ComparisonOp::Le,
                                    _ => {
                                        return None;
                                    }
                                },
                            });
                        }
                    }
                }
                // If we have something more fancy than "binary operator - identifier - number" i.e x < 5, y == 10,
                // Return None
                None
            }
            // Also, return None if it is not a binary operator in the expression
            _ => None,
        }
    }

    fn extract_inner_terms() -> Option<Vec<(i32, SymbolIdentifier)>> {
        None
    }

    // TODO when allowing more expressive expressions, this needs to be updated as well
    fn linear_program_simple_linear_expression(
        &self,
        linearexpression: &LinearConstraint,
        range_of_var: (f64, f64),
        direction: OptimizationDirection,
    ) -> Option<i32> {
        let mut problem = Problem::new(direction);
        let x = problem.add_var(1.0, (range_of_var.0, range_of_var.1));
        match linearexpression.operation {
            ComparisonOp::Eq => {
                problem.add_constraint(
                    &[(x, 1.0)],
                    ComparisonOp::Eq,
                    linearexpression.constant as f64,
                );
            }
            ComparisonOp::Ge => {
                problem.add_constraint(
                    &[(x, 1.0)],
                    ComparisonOp::Ge,
                    linearexpression.constant as f64,
                );
            }
            ComparisonOp::Le => {
                problem.add_constraint(
                    &[(x, 1.0)],
                    ComparisonOp::Le,
                    linearexpression.constant as f64,
                );
            }
        }
        match problem.solve() {
            Ok(solution) => Some(solution[x] as i32),
            Err(_) => None,
        }
    }

    /// Takes a Phi (the formula in the vertex) and maps it to a LinearConstrainedPhi, using the Ranges that we computed for the propositions
    fn map_phi_to_constraints(&self, phi: &Phi) -> LinearConstrainedPhi {
        match phi {
            Phi::True => LinearConstrainedPhi::True,
            Phi::False => LinearConstrainedPhi::False,
            Phi::Proposition(proposition) => {
                // If we get to a proposition, find the Range associated with it, in the proposition_cache and return this
                let constraint = self.proposition_cache.borrow().get(proposition).cloned();
                let bla = constraint.unwrap_or_else(|| {
                    // We have not tried to extract the linear expression from this proposition yet. Let's try
                    let decl = self.game.label_index_to_decl(*proposition);
                    if let DeclKind::Label(label) = &decl.kind {
                        let lin_expr = self.extract_outer_comparison(&label.condition.kind);
                        // Cache the result
                        self.proposition_cache
                            .borrow_mut()
                            .insert(*proposition, lin_expr.clone());
                        lin_expr
                    } else {
                        None
                    }
                });
                // Convert to LinearConstrainedPhi::Contraint if it is a linear expression, or true otherwise
                bla.map(|c| LinearConstrainedPhi::Constraint(c))
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
    fn visit_ranged_phi(&self, ranged_phi: &LinearConstrainedPhi, state: &State) -> Option<i32> {
        match ranged_phi {
            // If we need to satisfy either of the formulas, just return the lowest distance found between the two
            // If one the the sides is None, the other is returned (given that the other is Some)
            // This makes sure that "x < 5 || false" will not return None, but the dist in lhs
            LinearConstrainedPhi::Or(lhs, rhs) => {
                if let Some(lhs_distance) = self.visit_ranged_phi(lhs, state) {
                    return if let Some(rhs_distance) = self.visit_ranged_phi(rhs, state) {
                        Some(Ord::min(lhs_distance, rhs_distance))
                    } else {
                        Some(lhs_distance)
                    };
                } else if let Some(rhs_distance) = self.visit_ranged_phi(rhs, state) {
                    return Some(rhs_distance);
                }
                None
            }
            // If we need to satisfy both of the formulas, just return the largest distance found between the two
            LinearConstrainedPhi::And(lhs, rhs) => {
                if let Some(lhs_distance) = self.visit_ranged_phi(lhs, state) {
                    if let Some(rhs_distance) = self.visit_ranged_phi(rhs, state) {
                        return Some(Ord::max(lhs_distance, rhs_distance));
                    }
                }
                None
            }
            // Find distance to constraint
            LinearConstrainedPhi::Constraint(_constraint) => {
                // Some(self.distance_to_constraint(constraint, state_of_symbol)) // TODO calc distance to constraint
                Some(0)
            }
            LinearConstrainedPhi::True => Some(0),
            LinearConstrainedPhi::False => None,
        }
    }

    /// Returns how close we are to min or max in the Range
    fn distance_to_range_bound(&self, range: &Range<i32>, state_of_symbol: &i32) -> i32 {
        let dist_to_min = i32::abs(range.start - state_of_symbol);
        let dist_to_max = i32::abs(range.end - state_of_symbol);

        cmp::min(dist_to_min, dist_to_max)
    }
}
