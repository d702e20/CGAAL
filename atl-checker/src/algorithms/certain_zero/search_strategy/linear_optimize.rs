use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, ExprKind, Identifier};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
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
use crate::algorithms::certain_zero::search_strategy::linear_optimize::RangedPhi::Prop;

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
    proposition_cache: HashMap<usize, HashMap<SymbolIdentifier, Ranges>>,
    /// Maps a phi to a phi with ranges
    phi_cache: HashMap<u64, RangedPhi>,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Ranges {
    /// Must be the case that the state of the symbol is outside this range to satisfy the propositon
    NotRange(i32, i32),
    /// Must be the case that the state of the symbol is inside this range to satisfy the propositon (inclusive)
    Range(i32, i32),
}

/*#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum VecRanges {
    /// The state of the symbol must be within all the ranges in this to satisfy the formula
    AndVec(Vec<Ranges>),
    /// The state of the symbol must be within at least one of the ranges in this to satisfy the formula
    OrVec(Vec<Ranges>),
    HyperVec(Vec<VecRanges>),
}*/

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum RangedPhi {
    Or(Box<RangedPhi>, Box<RangedPhi>),
    And(Box<RangedPhi>, Box<RangedPhi>),
    Prop(HashMap<SymbolIdentifier, Ranges>),
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

    /// helper function to iterate through ATLVertices and find distance
    fn get_distance_in_atl_vertex(&mut self, target: &AtlVertex) -> Option<f32> {
        // TODO change edge by changing order of targets in the edge, based on distance

        // Hash phi, to see if we have seen it before by checking caches
        let mut hasher = DefaultHasher::new();
        target.formula().hash(&mut hasher);
        let phi_hash = hasher.finish();

        // TODO Should perhaps be removed. If we have seen this phi before, and this state, get the result instantly, not sure if this is possible?
        if let Some(distance) = self.result_cache.get(&(phi_hash, target.state())) {
            return Some(*distance as f32);
        } else {
            // todo could check if my current state is close to something in the cache, and just reuse that result, or perhaps look for more results, and extrapolate
        }

        // If we have not seen this phi before, find ranges for symbols that would satisfy it and update caches proposition cache
        if !self.phi_cache.contains_key(&phi_hash) {
            self.populate_formula_cache(target);
            self.phi_cache.insert(phi_hash, self.map_phi_to_ranges(&*target.formula()));
        }

        // The constructed RangedPhi
        if let Some(ranged_phi) = self.phi_cache.get(&phi_hash) {
            let state = self.game.state_from_index(target.state());
            let distance = self.visit_ranged_phi(ranged_phi, 10000, &state);
            self.result_cache.insert((phi_hash, target.state()), distance as i32);
            return Some(distance);
        }
        None
    }

    fn map_phi_to_ranges(&self, phi: &Phi) -> RangedPhi {
        match phi {
            Phi::True => { panic!("true not supported") }
            Phi::False => { panic!("false not supported") }
            Phi::Proposition(proposition) => {
                if let Some(symbol_range_map) = self.proposition_cache.get(proposition) {
                    return RangedPhi::Prop(symbol_range_map.clone());
                } else { panic!() }
            }
            Phi::Not(formula) => {
                let symbol_range_map = self.map_phi_to_ranges(formula);
                // Nut the RangedPhi
                return self.nut_ranged_phi(&symbol_range_map);
            }
            Phi::Or(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_ranges(lhs);
                let rhs_symbol_range_map = self.map_phi_to_ranges(rhs);

                return RangedPhi::Or(Box::from(lhs_symbol_range_map), Box::from(rhs_symbol_range_map));
            }
            Phi::And(lhs, rhs) => {
                let lhs_symbol_range_map = self.map_phi_to_ranges(lhs);
                let rhs_symbol_range_map = self.map_phi_to_ranges(rhs);

                return RangedPhi::And(Box::from(lhs_symbol_range_map), Box::from(rhs_symbol_range_map));
            }
            Phi::DespiteNext { formula, .. } => { return self.map_phi_to_ranges(formula); }
            Phi::EnforceNext { formula, .. } => { return self.map_phi_to_ranges(formula); }
            Phi::DespiteUntil { .. } => { panic!("Sorry, despite until not supported in los") }
            Phi::EnforceUntil { .. } => { panic!("Sorry, enforce until not supported in los") }
            Phi::DespiteEventually { formula, .. } => { return self.map_phi_to_ranges(formula); }
            Phi::EnforceEventually { formula, .. } => { return self.map_phi_to_ranges(formula); }
            Phi::DespiteInvariant { formula, .. } => { return self.map_phi_to_ranges(formula); }
            Phi::EnforceInvariant { formula, .. } => { return self.map_phi_to_ranges(formula); }
        }
    }

    fn nut_ranged_phi(&self, rangedphi: &RangedPhi) -> RangedPhi {
        return match rangedphi {
            RangedPhi::Or(lhs, rhs) => {
                RangedPhi::Or(Box::from(self.nut_ranged_phi(lhs)), Box::from(self.nut_ranged_phi(rhs)))
            }
            RangedPhi::And(lhs, rhs) => {
                RangedPhi::And(Box::from(self.nut_ranged_phi(lhs)), Box::from(self.nut_ranged_phi(rhs)))
            }
            RangedPhi::Prop(prop_hash_map) => {
                let mut nutted_prop_hash_map: HashMap<SymbolIdentifier, Ranges> = HashMap::new();

                for (symbol, range) in prop_hash_map {
                    match range {
                        NotRange(min, max) => {
                            nutted_prop_hash_map.insert(symbol.clone(), Range(*min, *max));
                        }
                        Range(min, max) => {
                            nutted_prop_hash_map.insert(symbol.clone(), NotRange(*min, *max));
                        }
                    }
                }
                Prop(nutted_prop_hash_map)
            }
        };
    }

    // todo instead of passing stuff, just get it each time?
    fn visit_ranged_phi(&self, ranged_phi: &RangedPhi, current_lowest_distance: i32, state: &State) -> f32 {
        match ranged_phi {
            RangedPhi::Or(lhs_prop_map, rhs_prop_map) => {
                let lhs_distance = self.visit_ranged_phi(lhs_prop_map, current_lowest_distance, state);
                let rhs_distance = self.visit_ranged_phi(rhs_prop_map, current_lowest_distance, state);

                // Todo dirty stuff here
                return (Ord::min(lhs_distance as i32, rhs_distance as i32)) as f32;
            }
            RangedPhi::And(lhs_prop_map, rhs_prop_map) => {
                let lhs_distance = self.visit_ranged_phi(lhs_prop_map, current_lowest_distance, state);
                let rhs_distance = self.visit_ranged_phi(rhs_prop_map, current_lowest_distance, state);

                // Todo dirty stuff here
                return (Ord::max(lhs_distance as i32, rhs_distance as i32)) as f32;
            }
            RangedPhi::Prop(prop_map) => {
                // todo cumulative, instead of max?
                let mut max_range = 100000;
                let mut updated = false;
                for (symbol, range) in prop_map {
                    if let Some(state_of_symbol) = state.0.get(symbol) {
                        if let Some(res) = self.find_lowest_distance_in_range(range, current_lowest_distance, state_of_symbol) {
                            if res < max_range {
                                max_range = res;
                                updated = true;
                            } else { continue; }
                        }
                    }
                }
                if updated {
                    return max_range as f32;
                } else { panic!("oh no") }
            }
        }
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
                                let res_hash = [(linear_expression.symbol, Ranges::Range(range.0, range.1))].iter().cloned().collect();
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