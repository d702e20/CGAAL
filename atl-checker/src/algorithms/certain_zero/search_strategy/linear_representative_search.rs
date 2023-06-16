use crate::algorithms::certain_zero::search_strategy::linear_constrained_phi::{
    all_variants, ConstrainedPhiMaker,
};
use crate::algorithms::certain_zero::search_strategy::linear_constraints::ComparisonOp;
use crate::algorithms::certain_zero::search_strategy::SearchStrategy;
use crate::atl::Phi;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::ast::DeclKind;
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use minilp::{LinearExpr, OptimizationDirection, Problem};
use priority_queue::PriorityQueue;
use std::collections::HashMap;

/// The [LinearRepresentativeSearch] is a search strategy that uses linear programming and
/// the LCGS's representation of states in order to prioritize the edges during the search.
/// However, it will only solve the linear programming problem once for the root formula,
/// and use the solution states as a representatives for the feasible regions of the formula.
/// Edges are then prioritized based on their manhattan distance to the representative states.
struct LinearRepresentativeSearch {
    game: IntermediateLcgs,
    queue: PriorityQueue<Edge<AtlVertex>, u32>,
    representatives: Vec<State>,
}

impl LinearRepresentativeSearch {
    fn new(game: IntermediateLcgs, representatives: Vec<State>) -> LinearRepresentativeSearch {
        LinearRepresentativeSearch {
            game,
            queue: PriorityQueue::new(),
            representatives,
        }
    }

    /// Find and use the representative states for the feasible regions of phi.
    fn set_representatives(&mut self, phi: &Phi) {
        self.representatives.clear();
        let linear_phi = ConstrainedPhiMaker::new().convert(&self.game, phi);
        for constraints in all_variants(&linear_phi) {
            let mut problem = Problem::new(OptimizationDirection::Minimize);

            // Add variable for each state variable
            let mut vars = HashMap::new();
            for state_var in self.game.get_vars() {
                let decl = self.game.get_decl(&state_var).unwrap();
                let DeclKind::StateVar(var_decl) = &decl.kind else { unreachable!() };
                let range = (
                    *var_decl.ir_range.start() as f64,
                    *var_decl.ir_range.end() as f64,
                );
                let var = problem.add_var(1.0, range);
                vars.insert(state_var, var);
            }

            // Add constraints based on phi
            for constraint in &constraints {
                if constraint.comparison == ComparisonOp::NotEqual {
                    // We skip not-equal constraints since the minilp cannot handle them.
                    // They don't restrict the solution space in a meaningful way anyway.
                    continue;
                }

                let mut lin_expr = LinearExpr::empty();
                for (state_var, coefficient) in &constraint.terms {
                    let goal_var = vars.get(state_var).unwrap();
                    lin_expr.add(*goal_var, *coefficient);
                }
                problem.add_constraint(
                    lin_expr,
                    constraint.comparison.into(),
                    -constraint.constant,
                );
            }

            // Extract solution
            if let Ok(solution) = problem.solve() {
                let mut state = State(HashMap::new());
                for (state_var, goal_var) in vars {
                    let v = solution[goal_var] as i32;
                    state.0.insert(state_var, v);
                }
                self.representatives.push(state);
            }
        }
    }

    /// Returns the distance between the given state and the closest representative state
    fn dist_to_representative(&self, state: &State) -> u32 {
        self.representatives
            .iter()
            .map(|rep| {
                let mut dist = 0;
                for state_var in self.game.get_vars() {
                    dist += (state.0[&state_var] - rep.0[&state_var]).unsigned_abs();
                }
                dist
            })
            .min()
            .unwrap_or(0) // If there are no representatives, then phi may not be satisfiable anyway
    }
}

impl SearchStrategy<AtlVertex> for LinearRepresentativeSearch {
    fn next(&mut self) -> Option<Edge<AtlVertex>> {
        self.queue.pop().map(|entry| entry.0)
    }

    fn queue_new_edges(&mut self, edges: Vec<Edge<AtlVertex>>) {
        for edge in edges {
            let state = self.game.state_from_index(edge.source().state());
            let dist = self.dist_to_representative(&state);
            self.queue.push(edge, u32::MAX - dist);
        }
    }
}
