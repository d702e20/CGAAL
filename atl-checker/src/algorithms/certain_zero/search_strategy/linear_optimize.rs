use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, ExprKind, Identifier};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::edg::{Edge, Vertex};
use crate::algorithms::certain_zero::search_strategy::{SearchStrategyBuilder, SearchStrategy};
use minilp::{Problem, OptimizationDirection, ComparisonOp, Error, Solution};
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
use std::sync::Arc;
use minilp::OptimizationDirection::{Maximize, Minimize};
use std::hash::{Hash, Hasher};
use crate::algorithms::certain_zero::search_strategy::linear_optimize::Ranges::{NotRange, Range};
use std::collections::hash_map::DefaultHasher;

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
    result_cache: HashMap<(u64, usize), i32>,
    formula_cache: HashMap<usize, HashMap<SymbolIdentifier, Vec<Ranges>>>,
    phi_cache: HashMap<u64, HashMap<SymbolIdentifier, Vec<Ranges>>>,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Ranges {
    NotRange(i32, i32),
    Range(i32, i32),
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLcgs) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: PriorityQueue::new(),
            game,
            result_cache: HashMap::new(),
            formula_cache: HashMap::new(),
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
        // Find the linear expression from the targets formula, if any
        //let linear_expressions = Some(vec![]);
        let mut hasher = DefaultHasher::new();
        target.formula().hash(&mut hasher);
        let phi_hash = hasher.finish();

        if let Some(distance) = self.result_cache.get(&(phi_hash, target.state())) {
            println!("found result in cache");
            return Some(*distance as f32);
        }

        if !self.phi_cache.contains_key(&phi_hash) {
            self.populate_formula_cache(target);
            let res = self.final_acceptance_formula(&*target.formula());
            self.phi_cache.insert(phi_hash, res);
        }

        if let Some(map) = self.phi_cache.get(&phi_hash) {
            let state = self.game.state_from_index(target.state());
            let mut distance: i32 = 100000000;
            for symbol_ranges in map {
                if let Some(state_of_symbol) = state.0.get(symbol_ranges.0) {
                    for range in symbol_ranges.1 {
                        match range {
                            NotRange(min, max) => {
                                if min < state_of_symbol || state_of_symbol < max {
                                    continue;
                                } else {
                                    let min_dist = min - state_of_symbol;
                                    let max_dist = max - state_of_symbol;
                                    if min_dist < max_dist {
                                        if min_dist < distance {
                                            distance = min_dist;
                                        }
                                    } else {
                                        if max_dist < distance {
                                            distance = max_dist;
                                        }
                                    }
                                }
                            }
                            Range(min, max) => {
                                if state_of_symbol < min || max < state_of_symbol {
                                    continue;
                                } else {
                                    let min_dist = min - state_of_symbol;
                                    let max_dist = max - state_of_symbol;
                                    if min_dist < max_dist {
                                        if min_dist < distance {
                                            distance = min_dist;
                                        }
                                    } else {
                                        if max_dist < distance {
                                            distance = max_dist;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return if distance != 100000000 {
                self.result_cache.insert((phi_hash, target.state()), distance);
                Some(distance as f32)
            } else { None };
        }
        None
    }

    fn final_acceptance_formula(&self, phi: &Phi) -> HashMap<SymbolIdentifier, Vec<Ranges>> {
        match phi {
            Phi::True => {}
            Phi::False => {}
            Phi::Proposition(x) => {
                if let Some(res) = self.formula_cache.get(x) {
                    return res.clone();
                } else { panic!() }
            }
            Phi::Not(formula) => {
                let mut new_hashmap: HashMap<SymbolIdentifier, Vec<Ranges>> = HashMap::new();
                let mut formula_hashmap = self.final_acceptance_formula(formula);

                for (symbol, ranges) in formula_hashmap.iter() {
                    let mut new_ranges: Vec<Ranges> = vec![];
                    for range in ranges {
                        match range {
                            Ranges::NotRange(min, max) => { new_ranges.push(Range(*min, *max)) }
                            Ranges::Range(min, max) => { new_ranges.push(NotRange(*min, *max)) }
                        };
                    }
                    new_hashmap.insert(symbol.clone(), new_ranges);
                }
                return new_hashmap;
            }
            Phi::Or(formula1, formula2) => {
                let mut new_hashmap: HashMap<SymbolIdentifier, Vec<Ranges>> = HashMap::new();

                let lhs_hashmap = self.final_acceptance_formula(formula1);
                let mut rhs_hashmap = self.final_acceptance_formula(formula2);

                for (symbol, lhs_range) in lhs_hashmap.iter() {
                    let mut new_ranges: Vec<Ranges> = vec![];
                    if let Some(rhs) = rhs_hashmap.get_key_value(symbol) {
                        // Combine ranges
                        let mut lb = lhs_range.clone();
                        let mut rb = rhs.1.clone();
                        lb.append(&mut rb);
                        new_ranges.append(&mut lb);
                        rhs_hashmap.remove(symbol);
                    } else {
                        new_ranges = lhs_range.clone();
                    }
                    new_hashmap.insert(symbol.clone(), new_ranges);
                }

                for (symbol, rhs_range) in rhs_hashmap.iter() {
                    new_hashmap.insert(symbol.clone(), rhs_range.clone());
                }
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
            if !self.formula_cache.contains_key(&proposition_index) {
                // Make sure it is a Label
                if let DeclKind::Label(label) = &self.game.label_index_to_decl(proposition_index).kind {
                    // Expression has to be linear
                    // Todo combine is_linear with extracted_linear_expression, perhaps also combine with this function
                    if true { //label.condition.is_linear() {
                        // Return the constructed Linear Expression from this condition
                        if let Some(linear_expression) = extract_linear_expression(label.condition.kind.clone()) {
                            if let Some(range) = self.get_linear_range(&linear_expression) {
                                let mut res_hash = HashMap::new();
                                res_hash.insert(linear_expression.symbol, vec![Ranges::Range(range.0, range.1)]);
                                self.formula_cache.insert(proposition_index, res_hash);
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
    use crate::game_structure::lcgs::parse::parse_lcgs;
    use crate::game_structure::lcgs::ast::Identifier::Simple;

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