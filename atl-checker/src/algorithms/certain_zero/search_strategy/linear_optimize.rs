use crate::algorithms::certain_zero::search_strategy::linear_optimize::Ranges::Range;
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
use std::cmp;
use std::collections::HashMap;
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
    /// Maps usize from proposition, to a hashmap mapping symbols to the ranges they need to be within,
    /// to satisfy the proposition
    proposition_cache: HashMap<Proposition, HashMap<SymbolIdentifier, Ranges>>,
    /// Maps hash of a phi to a rangedphi
    phi_cache: HashMap<Arc<Phi>, RangedPhi>,
    // TODO - could add a new cache, to compare distance between states and use this to guesstimate distance
    // TODO - to acceptance region, based on previous results (Mathias supervisor suggestion)
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLcgs) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: PriorityQueue::new(),
            game,
            result_cache: HashMap::new(),
            proposition_cache: HashMap::new(),
            phi_cache: HashMap::new(),
        }
    }
}

/// Holds the range for which a symbol should be within, to satisfy a proposition
#[derive(Clone)]
pub enum Ranges {
    /// Must be the case that the state of the symbol is inside this range to satisfy the propositon (inclusive)
    Range(i32, i32),
}

/// Used when mapping a Phi to a ranged variant,
/// where the propositions have been replaced by hashmaps mapping symbols to Ranges
pub enum RangedPhi {
    /// Either or should hold
    Or(Box<RangedPhi>, Box<RangedPhi>),
    /// Both should hold
    And(Box<RangedPhi>, Box<RangedPhi>),
    /// Mapping symbols to ranges
    Proposition(HashMap<SymbolIdentifier, Ranges>),
    True,
    False,
}

/// Holds extracted linear expressions from the formula in AtlVertex
struct LinearExpression {
    pub symbol: SymbolIdentifier,
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
                    self.get_distance_in_atl_vertex(target).map(|dist| distances.push(dist as f32));
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
                    Some(distance);
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
            // Find ranges for symbols that would satisfy it and update proposition_cache
            self.populate_proposition_cache(target);
            // Now we know the ranges for which the propositions in the formula would be satisfied,
            // Map the phi to one that holds Ranges, and cache this
            self.phi_cache
                .insert(target.formula(), self.map_phi_to_ranges(&*target.formula()));
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

    /// Finds all propositions in the formula in the vertex, and find Ranges for the symbols in them,
    /// that would satisfy the propositions, save these Ranges in cache
    fn populate_proposition_cache(&mut self, vertex: &AtlVertex) {
        // get all propositions from the formula in the vertex
        let propositions = vertex.formula().get_propositions_recursively();

        for proposition_index in propositions {
            // Only find Ranges for propositions that we have not seen before
            if !self.proposition_cache.contains_key(&proposition_index) {
                // Make sure it is a Label
                if let DeclKind::Label(label) =
                    &self.game.label_index_to_decl(proposition_index).kind
                {
                    // Return the constructed Linear Expression from this condition
                    if let Some(linear_expression) =
                        self.extract_linear_expression(label.condition.kind.clone())
                    {
                        // Find range for the symbol in the linear_expression to be within, to satisfy the given proposition
                        if let Some(range) = self.range_to_satisfy_proposition(&linear_expression) {
                            // Maps the symbol to the Range just found
                            let res_hash =
                                [(linear_expression.symbol, Ranges::Range(range.0, range.1))]
                                    .iter()
                                    .cloned()
                                    .collect();
                            // Add to proposition_cache
                            self.proposition_cache.insert(proposition_index, res_hash);
                        }
                    }
                }
            }
        }
    }

    // TODO should be replaced by something better
    /// Finds a single simple linear expression
    fn extract_linear_expression(&self, expr: ExprKind) -> Option<LinearExpression> {
        match &expr {
            ExprKind::BinaryOp(operator, operand1, operand2) => {
                // Either we have that operand1 is the owned ident, and operand2 is the constant
                if let ExprKind::OwnedIdent(id) = &operand1.kind {
                    if let Identifier::Resolved { owner, name } = *id.clone() {
                        let symbol_of_id = SymbolIdentifier {
                            owner: owner.clone(),
                            name: (name.clone()).parse().unwrap(),
                        };
                        if let ExprKind::Number(number) = operand2.kind {
                            return Some(LinearExpression {
                                symbol: symbol_of_id,
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
                    // Or we have that operand2 is the owned ident, and operand1 is the constant
                } else if let ExprKind::OwnedIdent(id) = &operand2.kind {
                    if let Identifier::Resolved { owner, name } = *id.clone() {
                        let symbol_of_id = SymbolIdentifier {
                            owner: owner.clone(),
                            name: (name.clone()).parse().unwrap(),
                        };
                        if let ExprKind::Number(number) = operand1.kind {
                            return Some(LinearExpression {
                                symbol: symbol_of_id,
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

    /// Runs the linear programming with both minimize, and maximize direction,
    /// To find the min and max value the symbol should be, to satisfy the proposition
    fn range_to_satisfy_proposition(
        &self,
        linear_expression: &LinearExpression,
    ) -> Option<(i32, i32)> {
        // Get the declaration from the symbol in LinearExpression, has to be a StateVar
        // (i.e a variable in an LCGS program)
        let symb = self.game.get_decl(&linear_expression.symbol).unwrap();
        if let DeclKind::StateVar(variable) = &symb.kind {
            // The range for the variable is used in the linear programming.
            // This range is the one declared in the LCGS program
            let range_of_var: (f64, f64) = (
                *variable.ir_range.start() as f64,
                *variable.ir_range.end() as f64,
            );

            // Find both min and max
            if let Some(min) = self.linear_program_simple_linear_expression(
                linear_expression,
                range_of_var,
                Minimize,
            ) {
                if let Some(max) = self.linear_program_simple_linear_expression(
                    linear_expression,
                    range_of_var,
                    Maximize,
                ) {
                    return Some((min, max));
                }
            }
        }
        // If we cannot find a min and max solution, return None
        None
    }

    // TODO when allowing more expressive expressions, this needs to be updated as well
    fn linear_program_simple_linear_expression(
        &self,
        linearexpression: &LinearExpression,
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

    /// Takes a Phi (the formula in the vertex) and maps it to a RangedPhi, using the Ranges that we computed for the propositions
    fn map_phi_to_ranges(&self, phi: &Phi) -> RangedPhi {
        match phi {
            Phi::True => {
                return RangedPhi::True;
            }
            Phi::False => {
                return RangedPhi::False;
            }
            Phi::Proposition(proposition) => {
                // If we get to a proposition, find the Range associated with it, in the proposition_cache and return this
                if let Some(symbol_range_map) = self.proposition_cache.get(proposition) {
                    return RangedPhi::Proposition(symbol_range_map.clone());
                } else {
                    panic!("Could not find range for proposition in cache, something went wrong with caching")
                }
            }
            Phi::Not(formula) => {
                return self.map_phi_to_ranges(formula);
            }
            Phi::Or(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_ranges(lhs);
                let rhs_symbol_range_map = self.map_phi_to_ranges(rhs);

                return RangedPhi::Or(
                    Box::from(lhs_symbol_range_map),
                    Box::from(rhs_symbol_range_map),
                );
            }
            Phi::And(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_ranges(lhs);
                let rhs_symbol_range_map = self.map_phi_to_ranges(rhs);

                return RangedPhi::And(
                    Box::from(lhs_symbol_range_map),
                    Box::from(rhs_symbol_range_map),
                );
            }
            Phi::DespiteNext { formula, .. } => {
                return self.map_phi_to_ranges(formula);
            }
            Phi::EnforceNext { formula, .. } => {
                return self.map_phi_to_ranges(formula);
            }
            Phi::DespiteUntil { pre, until, .. } => {
                let pre_symbol_range_map = self.map_phi_to_ranges(pre);
                let until_symbol_range_map = self.map_phi_to_ranges(until);

                return RangedPhi::Or(
                    Box::from(pre_symbol_range_map),
                    Box::from(until_symbol_range_map),
                );
            }
            Phi::EnforceUntil { pre, until, .. } => {
                let pre_symbol_range_map = self.map_phi_to_ranges(pre);
                let until_symbol_range_map = self.map_phi_to_ranges(until);

                return RangedPhi::Or(
                    Box::from(pre_symbol_range_map),
                    Box::from(until_symbol_range_map),
                );
            }
            Phi::DespiteEventually { formula, .. } => {
                return self.map_phi_to_ranges(formula);
            }
            Phi::EnforceEventually { formula, .. } => {
                return self.map_phi_to_ranges(formula);
            }
            Phi::DespiteInvariant { formula, .. } => {
                return self.map_phi_to_ranges(formula);
            }
            Phi::EnforceInvariant { formula, .. } => {
                return self.map_phi_to_ranges(formula);
            }
        }
    }

    /// Goes through the RangedPhi and finds how close we are to acceptance border in this state.
    fn visit_ranged_phi(&self, ranged_phi: &RangedPhi, state: &State) -> Option<i32> {
        match ranged_phi {
            // If we need to satisfy either of the formulas, just return the lowest distance found between the two
            // If one the the sides is None, the other is returned (given that the other is Some)
            // This makes sure that "x < 5 || false" will not return None, but the dist in lhs
            RangedPhi::Or(lhs, rhs) => {
                if let Some(lhs_distance) = self.visit_ranged_phi(lhs, state) {
                    return if let Some(rhs_distance) = self.visit_ranged_phi(rhs, state) {
                        Some(Ord::min(lhs_distance, rhs_distance))
                    } else {
                        Some(lhs_distance)
                    };
                } else {
                    if let Some(rhs_distance) = self.visit_ranged_phi(rhs, state) {
                        return Some(rhs_distance);
                    }
                }
                None
            }
            // If we need to satisfy both of the formulas, just return the largest distance found between the two
            RangedPhi::And(lhs, rhs) => {
                if let Some(lhs_distance) = self.visit_ranged_phi(lhs, state) {
                    if let Some(rhs_distance) = self.visit_ranged_phi(rhs, state) {
                        return Some(Ord::max(lhs_distance, rhs_distance));
                    }
                }
                None
            }
            // Iterate through all entires in the hashmap mapping symbols to ranges
            // Find distance for each symbol in the current state, to the closest acceptance region,
            // Add all distances and return
            RangedPhi::Proposition(proposition_range) => {
                let mut cumulative_distance = 0;

                for (symbol, range) in proposition_range {
                    if let Some(state_of_symbol) = state.0.get(symbol) {
                        let res = self.distance_to_range_bound(range, state_of_symbol);
                        cumulative_distance = cumulative_distance + res;
                    }
                }

                Some(cumulative_distance)
            }
            RangedPhi::True => Some(0),
            RangedPhi::False => None,
        }
    }

    /// Returns how close we are to min or max in the Range
    fn distance_to_range_bound(&self, range: &Ranges, state_of_symbol: &i32) -> i32 {
        return match range {
            Range(min, max) => {
                let dist_to_min = i32::abs(min - state_of_symbol);
                let dist_to_max = i32::abs(max - state_of_symbol);

                let min_distance = cmp::min(dist_to_min, dist_to_max);

                min_distance
            }
        };
    }
}
