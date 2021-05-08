use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, ExprKind, Identifier};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs};
use crate::edg::{Edge};
use crate::algorithms::certain_zero::search_strategy::{SearchStrategyBuilder, SearchStrategy};
use minilp::{Problem, OptimizationDirection, ComparisonOp};
use priority_queue::PriorityQueue;
use std::collections::HashMap;
use BinaryOpKind::{Addition,
                   Multiplication,
                   Subtraction,
                   Division,
                   Equality,
                   Inequality,
                   GreaterThan,
                   LessThan,
                   GreaterOrEqual,
                   LessOrEqual,
                   And,
                   Or,
                   Xor,
                   Implication, };
use crate::edg::atlcgsedg::AtlVertex;
use crate::atl::{Phi};
use minilp::OptimizationDirection::{Maximize, Minimize};
use std::hash::{Hash, Hasher};
use crate::algorithms::certain_zero::search_strategy::linear_optimize::Ranges::{NotRange, Range};
use std::collections::hash_map::DefaultHasher;
use std::cmp;

/// Holds extracted linear expressions from the formula in AtlVertex
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
struct LinearExpression {
    pub symbol: SymbolIdentifier,
    pub constant: i32,
    pub operation: BinaryOpKind,
}

/// Search strategy using ideas from linear programming to order next vertices,
/// based on distance from the vertex to a region that borders the line between true/false in the formula.
/// Has a cache to hold previously computed results, and uses a priority queue with
/// distances as priority (lowest distance has highest priority)
pub struct LinearOptimizeSearch {
    queue: PriorityQueue<Edge<AtlVertex>, i32>,
    game: IntermediateLcgs,
    /// Maps the hash of a formula and the usize representing a state, to a distance
    result_cache: HashMap<(u64, usize), i32>,
    /// Maps propositions (key) to a Hashmap of symbols, and the range the symbol should be within
    /// to satisfy the proposition
    proposition_cache: HashMap<usize, HashMap<SymbolIdentifier, VecRanges>>,
    /// Maps the hash of a formula to the final hashmap representing the state of the symbols
    /// and the ranges they should be within, to satisfy the entire formula Phi
    phi_cache: HashMap<u64, HashMap<SymbolIdentifier, VecRanges>>,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Ranges {
    /// Must be the case that the state of the symbol is outside this range to satisfy the propositon
    NotRange(i32, i32),
    /// Must be the case that the state of the symbol is inside this range to satisfy the propositon (inclusive)
    Range(i32, i32),
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum VecRanges {
    /// The state of the symbol must be within all the ranges in this to satisfy the formula
    AndVec(Vec<Ranges>),
    /// The state of the symbol must be within at least one of the ranges in this to satisfy the formula
    OrVec(Vec<Ranges>),
    HyperVec(Vec<VecRanges>),
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum RangedPhi {
    Or(VecRanges, VecRanges),
    And(VecRanges, VecRanges),
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

/// A SearchStrategyBuilder for building the LinearOptimizeSearch strategy.
pub struct LinearOptimizeSearchBuilder {
    pub game: IntermediateLcgs,
}

impl SearchStrategyBuilder<AtlVertex, LinearOptimizeSearch> for LinearOptimizeSearchBuilder {
    fn build(&self) -> LinearOptimizeSearch {
        LinearOptimizeSearch::new(self.game.clone())
    }
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
                self.queue.push(edge, -dist as i32);
            } else {
                // Todo what should default value be, if cannot be calculated?
                self.queue.push(edge, 0);
            }
        };
    }
}

impl LinearOptimizeSearch {
    /// if edge is a HyperEdge, return average distance from state to accept region between all targets,
    /// if Negation edge, just return the distance from its target
    fn get_distance_in_edge(&mut self, edge: &Edge<AtlVertex>) -> Option<f32> {
        match &edge {
            Edge::Hyper(hyperedge) => {
                // For every target of the hyperedge, we want to see how close we are to acceptance border
                let mut distances: Vec<f32> = Vec::new();
                for target in &hyperedge.targets {
                    if let Some(result) = self.get_distance_in_atl_vertex(target) {
                        distances.push(result)
                    }
                }

                // If no targets were able to satisfy formula, or something went wrong, return None
                return if distances.is_empty() {
                    None
                } else {
                    // Find average distance between targets, and return this
                    let avg_distance = distances.iter().sum::<f32>() / distances.len() as f32;
                    Some(avg_distance)
                };
            }
            // Same procedure for negation edges as for hyper, just no for loop for all targets, as we only have one target
            Edge::Negation(edge) => {
                self.get_distance_in_atl_vertex(&edge.target)
            }
        }
    }

    fn visit_ranged_phi(&self, ranged_phi: RangedPhi) -> i32 {
        0
    }

    /// helper function to iterate through ATLVertices and find distance
    fn get_distance_in_atl_vertex(&mut self, target: &AtlVertex) -> Option<f32> {
        // TODO change edge by changing order of targets in the edge, based on distance

        // Hash phi, to see if we have seen it before by checking caches
        let mut hasher = DefaultHasher::new();
        target.formula().hash(&mut hasher);
        let phi_hash = hasher.finish();

        // TODO Should perhaps be removed. If we have seen this phi before, and this state, get the result instantly, not sure if this is possible?
        if let Some(distance) = self.result_cache.get(&(phi_hash, target.state())) {
            println!("found result in cache");
            return Some(*distance as f32);
        }

        // If we have not seen this phi before, find ranges for symbols that would satisfy it and update caches
        if !self.phi_cache.contains_key(&phi_hash) {
            self.populate_formula_cache(target);
            self.phi_cache.insert(phi_hash, self.final_acceptance_formula(&*target.formula()));
        }

        // Now we know the ranges for symbols that would satisfy phi, as we know from either the cache from previously, or just updated it
        // Get the hashmap that maps the symbols to their ranges to satisfy phi
        if let Some(map) = self.phi_cache.get(&phi_hash) {
            // The current state in this vertex
            let state = self.game.state_from_index(target.state());
            // todo how do proper initialization
            let mut min_distance_in_state: i32 = 100000000;

            let res = 0;
            //let res = self.visit_ranged_phi(map);
            /*
            // For all mappings, find the minimum distance to satisfy for that specific symbol
            for symbol_ranges in map {
                // Get the actual i32 value of the state of the symbol
                if let Some(state_of_symbol) = state.0.get(symbol_ranges.0) {
                    // todo how do proper initialization
                    let mut min_dist_in_symbol: i32 = 100000;

                    // match the type of range the symbol should be within
                    if let Some(mut dist) = self.find_lowest_distance_in_vecrange(&symbol_ranges.1, min_dist_in_symbol, state_of_symbol) {
                        min_dist_in_symbol = dist;
                    }
                }
            }*/

            // Todo if we updated the min_distance_in_state, return this, else return None. Also update result_cache
            return if res != min_distance_in_state {
                self.result_cache.insert((phi_hash, target.state()), min_distance_in_state);
                Some(min_distance_in_state as f32)
            } else {
                None
            };
        }
        None
    }

    /// If AndRange, take highest distance found
    /// If OrRange, take lowest distance found
    fn find_lowest_distance_in_vecrange(&self, vecrange: &VecRanges, current_lowest_distance: i32, state_of_symbol: &i32) -> Option<i32> {
        match vecrange {
            VecRanges::AndVec(ranges) => {
                let mut result: i32 = 0;
                for range in ranges {
                    if let Some(res) = self.find_lowest_distance_in_range(&range, current_lowest_distance, state_of_symbol) {
                        if result < res {
                            result = res;
                        }
                    }
                }
                return Some(result);
            }
            VecRanges::OrVec(ranges) => {
                let mut result: i32 = 100000;
                for range in ranges {
                    if let Some(res) = self.find_lowest_distance_in_range(&range, current_lowest_distance, state_of_symbol) {
                        if res < result {
                            result = res;
                        }
                    }
                }
                return Some(result);
            }
            VecRanges::HyperVec(ranges) => {
                let mut result: i32 = 100000;
                for range in ranges {
                    if let Some(res) = self.find_lowest_distance_in_vecrange(range, current_lowest_distance, state_of_symbol) {
                        if res < result {
                            result = res;
                        }
                    }
                }
            }
        }
        None
    }


    fn find_lowest_distance_in_range(&self, range: &Ranges, current_lowest_distance: i32, state_of_symbol: &i32) -> Option<i32> {
        match range {
            NotRange(min, max) => {
                if state_of_symbol < min || max < state_of_symbol {
                    let dist_to_min = i32::abs(min - state_of_symbol);
                    let dist_to_max = i32::abs(max - state_of_symbol);

                    let min_distance = cmp::min(dist_to_min, dist_to_max);

                    return if min_distance < current_lowest_distance {
                        Some(min_distance)
                    } else { None };
                }
                None
            }
            Range(min, max) => {
                if min <= state_of_symbol || state_of_symbol <= max {
                    let dist_to_min = i32::abs(min - state_of_symbol);
                    let dist_to_max = i32::abs(max - state_of_symbol);

                    let min_distance = cmp::min(dist_to_min, dist_to_max);

                    return if min_distance < current_lowest_distance {
                        Some(min_distance)
                    } else { None };
                }
                None
            }
        }
    }

    fn nuttedvecranges(&self, ranges: &VecRanges) -> VecRanges {
        return match ranges {
            VecRanges::AndVec(ranges) => {
                let res = self.nuttedrange(&ranges);
                VecRanges::AndVec(res)
            }
            VecRanges::OrVec(ranges) => {
                let res = self.nuttedrange(&ranges);
                VecRanges::OrVec(res)
            }
            VecRanges::HyperVec(ranges) => {
                let mut ress: Vec<VecRanges> = Vec::new();
                for range in ranges {
                    let res = self.nuttedvecranges(range);
                    ress.push(res);
                }
                VecRanges::HyperVec(ress)
            }
        };
    }

    fn nuttedrange(&self, ranges: &Vec<Ranges>) -> Vec<Ranges> {
        let mut result: Vec<Ranges> = vec![];
        for range in ranges {
            match range {
                NotRange(min, max) => {
                    result.push(Range(*min, *max))
                }
                Range(min, max) => {
                    result.push(NotRange(*min, *max))
                }
            }
        }
        return result;
    }

    /// Muy importante function mapping the formula Phi to a map of symbols to ranges the symbols should be within to satisfy the formula
    fn final_acceptance_formula(&self, phi: &Phi) -> HashMap<SymbolIdentifier, VecRanges> {
        match phi {
            Phi::True => {}
            Phi::False => {}
            Phi::Proposition(x) => {
                if let Some(res) = self.proposition_cache.get(x) {
                    return res.clone();
                } else { panic!() }
            }
            Phi::Not(formula) => {
                // Get the hashmap from the formula inside the Not
                let formula_hashmap = self.final_acceptance_formula(formula);

                // Go through all entries and make Range into NotRange and vice versa
                let mut new_hashmap: HashMap<SymbolIdentifier, VecRanges> = HashMap::new();
                for (symbol, vecrange) in &formula_hashmap {
                    let new_vecranges: VecRanges = self.nuttedvecranges(&vecrange);
                    new_hashmap.insert(symbol.clone(), new_vecranges);
                }
                return new_hashmap;
            }
            Phi::Or(formula1, formula2) => {
                let mut new_hashmap: HashMap<SymbolIdentifier, VecRanges> = HashMap::new();

                // Need to combine these into a new OrVec
                let mut lhs_formula_hashmap = self.final_acceptance_formula(formula1);
                let mut rhs_formula_hashmap = self.final_acceptance_formula(formula2);

                /*for (symbol, vecrange) in &lhs_formula_hashmap {
                    let new_symbol_entry: (SymbolIdentifier, VecRanges);

                    // do stuff
                    for (symbol, lhs_range) in lhs_formula_hashmap {
                        if let Some(rhs_range) = rhs_formula_hashmap.get_key_value(&symbol) {
                            // add to new hash map

                            rhs_formula_hashmap.remove(&symbol);
                            lhs_formula_hashmap.remove(&symbol);
                        } else {
                            // Only found in left

                            // add to new hashmap

                            lhs_formula_hashmap.remove(&symbol);
                        }
                    }
                }
                // Those only found in lhs
                for (symbol, lhs_range) in lhs_formula_hashmap {


                    // add to new hash map
                }

                // those only found in rhs
                for (symbol, lhs_range) in rhs_formula_hashmap {
                    // add to new hash map
                }

                new_hashmap.insert(symbol.clone(), new_vecranges);*/

                return new_hashmap;
            }

            Phi::And(formula1, formula2) => {
                let res1 = self.final_acceptance_formula(formula1);
                let res2 = self.final_acceptance_formula(formula2);
            }
            Phi::DespiteNext { formula, .. } => { return self.final_acceptance_formula(formula); }
            Phi::EnforceNext { formula, .. } => { return self.final_acceptance_formula(formula); }
            Phi::DespiteUntil { pre, until, .. } => { panic!("sorry, los does not support untils (yet)") }
            Phi::EnforceUntil { pre, until, .. } => { panic!("sorry, los does not support untils (yet)") }
            Phi::DespiteEventually { formula, .. } => { return self.final_acceptance_formula(formula); }
            Phi::EnforceEventually { formula, .. } => { return self.final_acceptance_formula(formula); }
            Phi::DespiteInvariant { formula, .. } => { return self.final_acceptance_formula(formula); }
            Phi::EnforceInvariant { formula, .. } => { return self.final_acceptance_formula(formula); }
        }
        panic!("what the fuck did you do");
    }

    fn populate_formula_cache(&mut self, vertex: &AtlVertex) {
        // get propositions from the formula in the vertex
        let propositions = vertex.formula().get_propositions_recursively();

        for proposition_index in propositions.into_iter() {
            if !self.proposition_cache.contains_key(&proposition_index) {
                // Make sure it is a Label
                if let DeclKind::Label(label) = &self.game.label_index_to_decl(proposition_index).kind {
                    // Expression has to be linear
                    if true { //label.condition.is_linear() {
                        // Return the constructed Linear Expression from this condition
                        if let Some(linear_expression) = extract_linear_expression(label.condition.kind.clone()) {
                            if let Some(range) = self.get_linear_range(&linear_expression) {
                                let res_hash = [(linear_expression.symbol, VecRanges::OrVec(vec![Ranges::Range(range.0, range.1)]))].iter().cloned().collect();
                                self.proposition_cache.insert(proposition_index, res_hash);
                            }
                        }
                    }
                }
            }
        }
    }

    fn lin_prog(&self, range_of_var: (f64, f64), constant: f64, operation: BinaryOpKind, direction: OptimizationDirection) -> Option<i32> {
        let mut problem = Problem::new(direction);
        let x = problem.add_var(1.0, (range_of_var.0, range_of_var.1));
        // TODO support for more operators?
        match operation {
            Addition => { return None; }
            Multiplication => { return None; }
            Subtraction => { return None; }
            Division => { return None; }
            Equality => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Eq, constant); }
            Inequality => { return None; }
            GreaterThan => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Ge, constant); }
            LessThan => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Le, constant); }
            GreaterOrEqual => { return None; }
            LessOrEqual => { return None; }
            And => { return None; }
            Or => { return None; }
            Xor => { return None; }
            Implication => { return None; }
        }
        match problem.solve() {
            Ok(solution) => { Some(solution[x] as i32) }
            Err(_) => { None }
        }
    }

    fn get_linear_range(&self, linear_expression: &LinearExpression) -> Option<(i32, i32)> {
        // Get the declaration from the symbol in LinearExpression, has to be a StateVar
        // (i.e a variable in an LCGS program)
        let symb = self.game.get_decl(&linear_expression.symbol).unwrap();
        if let DeclKind::StateVar(var) = &symb.kind {

            // The range is used for linear programming
            let range_of_var: (f64, f64) = (*var.ir_range.start() as f64, *var.ir_range.end() as f64);

            if let Some(min) = self.lin_prog(range_of_var, linear_expression.constant as f64, linear_expression.operation.clone(), Minimize) {
                if let Some(max) = self.lin_prog(range_of_var, linear_expression.constant as f64, linear_expression.operation.clone(), Maximize) {
                    return Some((min, max));
                }
            }
        }
        None
    }
}

// Todo combine this with expr.is_linear() in a visitor pattern, is currently very MVP and only extracts x < 5 and such,
// TODO is next in line to be rewritten
fn extract_linear_expression(expr: ExprKind) -> Option<LinearExpression> {
    match &expr {
        ExprKind::BinaryOp(operator, operand1, operand2) => {
            if let ExprKind::OwnedIdent(id) = &operand1.kind {
                if let Identifier::Resolved { owner, name } = *id.clone() {
                    let symbol_of_id = SymbolIdentifier { owner: owner.clone(), name: (name.clone()).parse().unwrap() };
                    if let ExprKind::Number(number) = operand2.kind {
                        return Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: operator.clone() });
                    }
                }
                // 2nd case
            } else if let ExprKind::OwnedIdent(id) = &operand2.kind {
                if let Identifier::Resolved { owner, name } = *id.clone() {
                    let symbol_of_id = SymbolIdentifier { owner: owner.clone(), name: (name.clone()).parse().unwrap() };
                    if let ExprKind::Number(number) = operand1.kind {
                        return Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: operator.clone() });
                    }
                }
            }
            None
        }
        _ => { None }
    }
}

mod test {
    use crate::game_structure::lcgs::ast::BinaryOpKind::{Addition, Multiplication};
    use crate::game_structure::lcgs::ast::{Expr, BinaryOpKind};
    use crate::game_structure::lcgs::ir::symbol_table::{SymbolIdentifier, Owner};
    use crate::algorithms::certain_zero::search_strategy::linear_optimize::{LinearExpression, LinearOptimizeSearch};
    use crate::game_structure::lcgs::ast::ExprKind::{Number, BinaryOp, OwnedIdent};
    use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
    use crate::game_structure::lcgs::ast::Identifier::Simple;
    use crate::game_structure::lcgs::parse::parse_lcgs;

    #[test]
    // 1 + 1
    fn expression_is_linear_test_two_numbers() {
        let operator = Addition;
        let operand1 = Box::from(Expr { kind: Number(1) });
        let operand2 = Box::from(Expr { kind: Number(1) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression.is_linear(), true)
    }

    #[test]
    // 1 * 1
    fn expression_is_linear_test_two_numbers1() {
        let operator = Multiplication;
        let operand1 = Box::from(Expr { kind: Number(1) });
        let operand2 = Box::from(Expr { kind: Number(1) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression.is_linear(), true)
    }

    #[test]
    // x * x
    fn expression_is_linear_test_two_variables() {
        let operator = Multiplication;
        let operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression.is_linear(), false)
    }

    #[test]
    #[ignore]
    /// currently not working because of faulty is_linear
    // x * y
    fn expression_is_linear_test_two_variables1() {
        let operator = Multiplication;
        let operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "y".to_string() })) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression.is_linear(), false)
    }

    #[test]
    // x + x
    fn expression_is_linear_test_two_variables2() {
        let operator = Addition;
        let operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression.is_linear(), true)
    }

    #[test]
    // 5 + x * 3
    fn expression_is_linear_test_simple_linear() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let inner_operand2 = Box::from(Expr { kind: Number(3) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: Number(5) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression.is_linear(), true)
    }

    #[test]
    // x + 3 * 3
    fn expression_is_linear_test_linear_same_constants_in_mult() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: Number(3) });
        let inner_operand2 = Box::from(Expr { kind: Number(3) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression.is_linear(), true)
    }

    #[test]
    // 5 + x * x
    fn expression_is_linear_test_polynomial() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let inner_operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: Number(5) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression.is_linear(), false)
    }

    #[test]
    // 5 * x * x
    fn expression_is_linear_test_polynomial1() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let inner_operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });

        let middle_operator = Multiplication;
        let middle_operand1 = Box::from(Expr { kind: Number(5) });
        let middle_operand2 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(middle_operator, middle_operand1, middle_operand2) });
        let outer_operand2 = Box::from(Expr { kind: Number(5) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression.is_linear(), false)
    }

    #[test]
    #[ignore]
    /// Currently returns wrong result, because of is_linear need to be rewritten
    // x * x * 5
    fn expression_is_linear_test_polynomial2() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let inner_operand2 = Box::from(Expr { kind: Number(5) });

        let outer_operator = Multiplication;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression.is_linear(), false)
    }

    // TODO write more tests
    #[test]
    fn minimum_distance_1d_test_lessthan() {
        // Are the expected labels present
        let input = "
        x : [0 .. 9] init 0;
        x' = x;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let initial = lcgs.initial_state();

        let lin_exp = LinearExpression {
            symbol: SymbolIdentifier { owner: Owner::Global, name: "x".to_string() },
            constant: 5,
            operation: BinaryOpKind::LessThan,
        };

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), &lin_exp);
        let expected = 0.0;
        assert_eq!(solution.unwrap(), expected);
    }

    #[test]
    fn minimum_distance_1d_test_equality() {
        // Are the expected labels present
        let input = "
        x : [0 .. 9] init 0;
        x' = x;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let initial = lcgs.initial_state();

        let lin_exp = LinearExpression {
            symbol: SymbolIdentifier { owner: Owner::Global, name: "x".to_string() },
            constant: 5,
            operation: BinaryOpKind::Equality,
        };

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), &lin_exp);
        let expected = 5.0;
        assert_eq!(solution.unwrap(), expected);
    }

    #[test]
    fn minimum_distance_1d_test_greaterthan() {
        // Are the expected labels present
        let input = "
        x : [0 .. 9] init 0;
        x' = x;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let initial = lcgs.initial_state();

        let lin_exp = LinearExpression {
            symbol: SymbolIdentifier { owner: Owner::Global, name: "x".to_string() },
            constant: 5,
            operation: BinaryOpKind::GreaterThan,
        };

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), &lin_exp);
        let expected = 5.0;
        assert_eq!(solution.unwrap(), expected);
    }

    #[test]
    fn minimum_distance_1d_test_nonexisting_operation() {
        // Are the expected labels present
        let input = "
        x : [0 .. 9] init 0;
        x' = x;
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        let initial = lcgs.initial_state();

        let lin_exp = LinearExpression {
            symbol: SymbolIdentifier { owner: Owner::Global, name: "x".to_string() },
            constant: 5,
            operation: BinaryOpKind::Implication,
        };

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), &lin_exp);
        assert!(solution.is_none());
    }
}

// Todo list
// fix translating phi to ranges
// fix default values/initialization