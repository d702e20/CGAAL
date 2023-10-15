use crate::algorithms::certain_zero::search_strategy::linear_constrained_phi::{
    all_variants, ConstrainedPhiMaker, LinearConstrainedPhi,
};
use crate::algorithms::certain_zero::search_strategy::linear_constraints::ComparisonOp;
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::Phi;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::Edge;
use minilp::{LinearExpr, OptimizationDirection, Problem, Variable};
use priority_queue::PriorityQueue;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::option::Option::Some;
use std::sync::Arc;
use crate::game_structure::lcgs::intermediate::{IntermediateLcgs, State};
use crate::game_structure::lcgs::symbol_table::SymbIdx;
use crate::parsing::ast::DeclKind;

/// A SearchStrategyBuilder for building the LinearProgrammingSearch strategy.
pub struct LinearProgrammingSearchBuilder {
    pub game: IntermediateLcgs,
}

impl SearchStrategyBuilder<AtlVertex, LinearProgrammingSearch> for LinearProgrammingSearchBuilder {
    fn build(&self, _root: &AtlVertex) -> LinearProgrammingSearch {
        LinearProgrammingSearch::new(self.game.clone())
    }
}

/// Search strategy using ideas from linear programming to order next vertices,
/// based on distance from the vertex to a region that borders the line between true/false in the formula.
pub struct LinearProgrammingSearch {
    /// Priority based on distance, lowest distance highest priority
    queue: PriorityQueue<Edge<AtlVertex>, i32>,
    game: IntermediateLcgs,
    constrained_phi_maker: ConstrainedPhiMaker,
    /// Caches the linear programming solution state and distance of ATL vertices
    result_cache: HashMap<AtlVertex, i32>,
    /// Caches the LinearConstrainedPhi version of Phis
    phi_cache: HashMap<Arc<Phi>, LinearConstrainedPhi>,
    // TODO - could add a new cache, to compare distance between states and use this to guesstimate distance
    // TODO - to acceptance region, based on previous results (Mathias supervisor suggestion)
}

impl LinearProgrammingSearch {
    pub fn new(game: IntermediateLcgs) -> LinearProgrammingSearch {
        LinearProgrammingSearch {
            queue: PriorityQueue::new(),
            game,
            constrained_phi_maker: ConstrainedPhiMaker::new(),
            result_cache: HashMap::new(),
            phi_cache: HashMap::new(),
        }
    }
}

impl SearchStrategy<AtlVertex> for LinearProgrammingSearch {
    /// Simply returns the edge with highest priority (i.e lowest distance)
    fn next(&mut self) -> Option<Edge<AtlVertex>> {
        self.queue.pop().map(|(edge, _)| edge)
    }

    /// Takes a Vec of Edges holding ATLVertices, and puts these in the queue,
    /// based on the calculated distance from its state to acceptance region from formula
    fn queue_new_edges(&mut self, edges: Vec<Edge<AtlVertex>>) {
        for edge in edges {
            let distance = self.get_distance_of_edge(&edge);

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

impl LinearProgrammingSearch {
    /// Finds the distance of an edge using linear programming
    fn get_distance_of_edge(&mut self, edge: &Edge<AtlVertex>) -> Option<i32> {
        let source = edge.source();

        // If we have seen this source before, get the result instantly
        if let Some(dist) = self.result_cache.get(source) {
            return Some(*dist);
        }

        // If we have not seen this formula before, calculate constrained phi
        if let Entry::Vacant(e) = self.phi_cache.entry(source.formula()) {
            // Convert the formula to a structure of constraints
            let lcp = self
                .constrained_phi_maker
                .convert(&self.game, &source.formula());
            e.insert(lcp);
        }

        // Get constraints of this phi
        let constrained_phi = self.phi_cache.get(&source.formula()).unwrap();
        // Get current state in vertex
        let state = self.game.state_from_index(source.state());

        // Use linear programming to find closest state that satisfies the constraints
        if let Some(dist) = self.get_optimal_distance(constrained_phi, &state) {
            // Cache the resulting solution
            self.result_cache.insert(source.clone(), dist);
            return Some(dist);
        }
        // If we could not find a distance, return None
        None
    }

    /// Returns the small distance from the given state to any state that satisfies
    /// the given constrained phi, or None if no such state exists. NonLinear constraints
    /// of phi is not considered.
    fn get_optimal_distance(
        &self,
        constrained_phi: &LinearConstrainedPhi,
        state: &State,
    ) -> Option<i32> {
        // The LinearConstrainedPhi may contain multiple variants of the linear problem due to disjunctions,
        // so we iterate through them and find the closest distance across all problems
        let mut best: Option<i32> = None;
        for constraints in all_variants(constrained_phi) {
            let mut problem = Problem::new(OptimizationDirection::Minimize);

            // Every state variable is associated with two linear programming variables:
            // - goal_var which represents the variable in the closest state satisfied the constraints, and
            // - clamp_var which forces the value of goal_var to be close to the value in the current state
            let mut symbol_vars: HashMap<SymbIdx, Variable> = HashMap::new();
            for state_var in self.game.get_vars() {
                let decl = self.game.get_decl(state_var).unwrap();
                let DeclKind::StateVar(var_decl) = &decl.kind else {
                    unreachable!()
                };
                let range = (
                    *var_decl.range.val.start() as f64,
                    *var_decl.range.val.end() as f64,
                );
                let goal_var = problem.add_var(0.0, range);
                let clamp_var = problem.add_var(1.0, (f64::NEG_INFINITY, f64::INFINITY));
                symbol_vars.insert(*state_var, goal_var);

                let cur_val = state.0[state_var] as f64;

                // Add the constraints that forces the difference between goal_var and state_var to be small
                // - x_i - s_i <= - q_i
                let mut lin_expr_below = LinearExpr::empty();
                lin_expr_below.add(goal_var, -1.0);
                lin_expr_below.add(clamp_var, -1.0);
                problem.add_constraint(lin_expr_below, minilp::ComparisonOp::Le, -cur_val);
                // - x_i + s_i <= q_i
                let mut lin_expr_above = LinearExpr::empty();
                lin_expr_above.add(goal_var, 1.0);
                lin_expr_above.add(clamp_var, -1.0);
                problem.add_constraint(lin_expr_above, minilp::ComparisonOp::Le, cur_val);
            }

            // Now add every constraint induced by formula phi
            for constraint in &constraints {
                if constraint.comparison == ComparisonOp::NotEqual {
                    // We skip not-equal constraints since the minilp cannot handle them.
                    // They don't restrict the solution space in a meaningful way anyway.
                    continue;
                }

                // Build constraint on goal state
                let mut lin_expr = LinearExpr::empty();
                for (state_var, coefficient) in &constraint.terms {
                    let goal_var = symbol_vars.get(state_var).unwrap();
                    lin_expr.add(*goal_var, *coefficient);
                }
                problem.add_constraint(
                    lin_expr,
                    constraint.comparison.into(),
                    -constraint.constant,
                );
            }

            if let Ok(solution) = problem.solve() {
                // Extract solution
                let mut man_dist = 0;
                for (state_var, goal_var) in symbol_vars {
                    let v = solution[goal_var] as i32;
                    man_dist += (state.0[&state_var] - v).abs();
                }

                if let Some(old_best_dist) = best {
                    if old_best_dist > man_dist {
                        // We found a better solution
                        best = Some(man_dist);
                    }
                } else {
                    // We got our first possible a solution
                    best = Some(man_dist);
                }
            }
        }

        best
    }
}
