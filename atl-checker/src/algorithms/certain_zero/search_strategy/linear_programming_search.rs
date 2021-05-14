use crate::algorithms::certain_zero::search_strategy::linear_constraints::{
    LinearConstraint, LinearConstraintExtractor,
};
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::Phi;
use crate::edg::atlcgsedg::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, UnaryOpKind};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::Proposition;
use crate::game_structure::State as StateUsize;
use float_ord::FloatOrd;
use minilp::{LinearExpr, OptimizationDirection, Problem, Variable};
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
    /// Maps the hash of a Phi and usize of state, to a result state and distance
    result_cache: HashMap<AtlVertex, (State, i32)>,
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

impl LinearOptimizeSearch {
    /// Finds the distance of an edge using linear programming
    fn get_distance_of_edge(&mut self, edge: &Edge<AtlVertex>) -> Option<i32> {
        let source = edge.source();

        // If we have seen this source before, get the result instantly
        if let Some(sol) = self.result_cache.get(&source) {
            return Some(sol.1);
        }

        // If we have not seen this formula before, calculate constrained phi
        if !self.phi_cache.contains_key(&source.formula()) {
            // Convert the formula to a structure of constraints
            // TODO
            // self.phi_cache.insert(
            //     target.formula(),
            //     self.map_phi_to_constraints(&*target.formula()),
            // );
        }

        // Get constraints of this phi
        let constrained_phi = self.phi_cache.get(&source.formula()).unwrap();
        // Get current state in vertex
        let state = self.game.state_from_index(source.state());

        // Use linear programming to find closest state that satisfies the constraints
        if let Some(sol) = self.get_optimal_distance(constrained_phi, &state) {
            let dist = sol.1;
            // Cache the resulting solution
            self.result_cache.insert(source.clone(), sol);
            return Some(dist);
        }
        // If we could not find a distance, return None
        None
    }

    fn get_optimal_distance(
        &self,
        constrained_phi: &LinearConstrainedPhi,
        state: &State,
    ) -> Option<(State, i32)> {
        // The LinearConstrainedPhi contains multiple variants of the linear problem due to the ORs.
        // So we iterate through them and find the best result
        let mut best: Option<(State, i32)> = None;
        for constraints in LinearProblemConstraintIterator::new(constrained_phi) {
            // We keep track of the symbols/variables encountered
            let mut symbol_vars: HashMap<SymbolIdentifier, Variable> = HashMap::new();
            let mut problem = Problem::new(OptimizationDirection::Minimize);

            for constraint in &constraints {
                // Build constraint
                let mut lin_expr = LinearExpr::empty();
                // The contraint is offset by the given state. This is reflected by adding
                // coefficient*value_of_symbol_in_state to the constant
                let mut offset = -constraint.constant;
                for (symbol, coefficient) in &constraint.terms {
                    // Get symbol variable or register if missing
                    let var = symbol_vars.entry(symbol.clone()).or_insert_with(|| {
                        let decl = self.game.get_decl(symbol).unwrap();
                        if let DeclKind::StateVar(var_decl) = &decl.kind {
                            let range = (
                                *var_decl.ir_range.start() as f64,
                                *var_decl.ir_range.end() as f64,
                            );
                            problem.add_var(1.0, range)
                        } else {
                            panic!("Proposition contains a non-variable")
                        }
                    });
                    lin_expr.add(*var, *coefficient);
                    offset += *coefficient * state.0[&symbol] as f64;
                }

                problem.add_constraint(lin_expr, constraint.comparison.into(), offset);
            }

            if let Ok(solution) = problem.solve() {
                // Extract solution
                let mut sol_state: HashMap<SymbolIdentifier, i32> = HashMap::new();
                let mut man_dist = 0;
                for (symbol, var) in symbol_vars {
                    let v = solution[var] as i32;
                    man_dist += (state.0[&symbol] - v).abs();
                    sol_state.insert(symbol, v);
                }

                if let Some((_, old_best_dist)) = best {
                    if old_best_dist > man_dist {
                        // We found a better solution
                        best = Some((State(sol_state), man_dist));
                    }
                } else {
                    // We our first possible a solution
                    best = Some((State(sol_state), man_dist));
                }
            }
        }

        best
    }
}

struct LinearProblemConstraintIterator<'a> {
    phi: &'a LinearConstrainedPhi,
}

impl<'a> LinearProblemConstraintIterator<'a> {
    fn new(phi: &'a LinearConstrainedPhi) -> LinearProblemConstraintIterator<'a> {
        LinearProblemConstraintIterator { phi }
    }
}

impl<'a> Iterator for LinearProblemConstraintIterator<'a> {
    type Item = Vec<LinearConstraint>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
