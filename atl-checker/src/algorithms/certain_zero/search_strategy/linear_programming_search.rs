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
    queue: PriorityQueue<Edge<AtlVertex>, FloatOrd<f64>>,
    game: IntermediateLcgs,
    /// Maps the hash of a Phi and usize of state, to a result distance
    result_cache: HashMap<(Arc<Phi>, StateUsize), f64>,
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
                self.queue.push(edge, FloatOrd(-dist));
            } else {
                // Todo what should default value be, if cannot be calculated? For now: first priority
                self.queue.push(edge, FloatOrd(0.0));
            }
        }
    }
}

impl LinearOptimizeSearch {
    /// Finds the distance in a single atl_vertex
    fn get_distance_in_atl_vertex(&mut self, target: &AtlVertex) -> Option<f64> {
        // If we have seen this phi before, and this state, get the result instantly
        if let Some(distance) = self.result_cache.get(&(target.formula(), target.state())) {
            return Some(*distance);
        }

        // If we have not seen this formula before
        if !self.phi_cache.contains_key(&target.formula()) {
            // Convert the formula to a structure of constraints
            // TODO
            // self.phi_cache.insert(
            //     target.formula(),
            //     self.map_phi_to_constraints(&*target.formula()),
            // );
        }

        // Get constraints of this phi
        let constrained_phi = self.phi_cache.get(&target.formula()).unwrap();
        // Get current state in vertex
        let state = self.game.state_from_index(target.state());

        // Find the distance to constraints by visiting the constrained phi
        if let Some(distance) = self.get_optimal_distance(constrained_phi, &state) {
            // Cache the resulting distance from the combination of this formula and state
            self.result_cache
                .insert((target.formula(), target.state()), distance);
            // Return calculated distance
            return Some(distance);
        }
        // If we could not find a distance, return None
        None
    }

    fn get_optimal_distance(
        &self,
        constrained_phi: &LinearConstrainedPhi,
        state: &State,
    ) -> Option<f64> {
        // The LinearConstrainedPhi contains multiple variants of the linear problem due to the ORs.
        // So we iterate through them and find the best result
        let best = None
        for constraints in LinearProblemConstraintIterator::new(constrained_phi) {
            // We keep track of the symbols/variables encountered
            let mut symbol_vars: HashMap<SymbolIdentifier, Variable> = HashMap::new();
            let mut problem = Problem::new(OptimizationDirection::Minimize);

            for constraint in &constraints {
                // Build constraint
                let mut lin_expr = LinearExpr::empty();
                for (symbol, coefficient) in &constraint.terms {
                    // Get symbol variable or register if missing
                    let var = symbol_vars.get(&symbol).get_or_insert_with(|s| {
                        &problem.add_var(1.0, (f64::MIN, f64::MAX)) // TODO Use symbol range from LCGS
                    });
                    lin_expr.add(**var, *coefficient);
                }

                problem.add_constraint(
                    lin_expr,
                    constraint.comparison.into(),
                    -constraint.constant,
                );
            }

            if let Ok(solution) = problem.solve() {
                // Extract solution
                // TODO
            }
        }
        None
    }
}

struct LinearProblemConstraintIterator<'a> {}

impl<'a> LinearProblemConstraintIterator<'a> {
    fn new(phi: &'a LinearConstrainedPhi) -> LinearProblemConstraintIterator<'a> {
        LinearProblemIterator
    }
}

impl Iterator for LinearProblemConstraintIterator {
    type Item = Vec<LinearConstraint>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
