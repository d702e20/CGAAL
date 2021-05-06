use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, ExprKind, Identifier};
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::edg::{Edge, Vertex};
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

/// Holds extracted linear expressions from the formula in AtlVertex
#[derive(Eq, PartialEq, Hash, Clone)]
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
    cache: HashMap<(Vec<LinearExpression>, usize), i32>,
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLcgs) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: PriorityQueue::new(),
            game,
            cache: HashMap::new(),
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
                    if let Some(result) = self.get_distance_in_AtlVertex(target) {
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
                self.get_distance_in_AtlVertex(&edge.target)
            }
        }
    }

    /// helper function to iterate through ATLVertices and find distance
    fn get_distance_in_AtlVertex(&mut self, target: &AtlVertex) -> Option<f32> {
        // TODO change edge by changing order of targets in the edge, based on distance
        // Find the linear expression from the targets formula, if any
        let linear_expressions = self.get_linear_expressions_from_AtlVertex(target);

        // Polynomials and such not allowed, returns None in such cases
        if let Some(expressions) = linear_expressions {
            // get the State in the target
            let state = self.game.state_from_index(target.state());

            // Check if result is in cache
            if let Some(cached_result) = self.cache.get(&(expressions.clone(), target.state())) {
                return Some(*cached_result as f32);
            }

            let mut expr_distance: f32 = 0.0;
            for linear_expression in &expressions {
                // Distance from the state, to fulfilling the linear expression
                let distance_to_solve_expression = self.minimum_distance_1d(state.clone(), linear_expression);
                if let Some(distance) = distance_to_solve_expression {
                    expr_distance = expr_distance + distance;
                }
            }
            // If we got results, add to vec of distances and cache
            // TODO expand cache with more results?
            if 0.0 < expr_distance {
                self.cache.insert((expressions.clone(), target.state()), expr_distance as i32);
                return Some(expr_distance / expressions.len() as f32);
            }
        }
        None
    }

    fn get_linear_expressions_from_AtlVertex(&self, vertex: &AtlVertex) -> Option<Vec<LinearExpression>> {
        // get propositions from the formula in the vertex
        let propositions = vertex.formula().get_propositions_recursively();

        let mut linear_expressions: Vec<LinearExpression> = Vec::new();
        for proposition_index in propositions.into_iter() {
            // Make sure it is a Label
            if let DeclKind::Label(label) = &self.game.label_index_to_decl(proposition_index).kind {
                // Expression has to be linear
                // Todo combine is_linear with extracted_linear_expression, perhaps also combine with this function
                if label.condition.is_linear() {
                    // Return the constructed Linear Expression from this condition
                    if let Some(linear_expression) = extract_linear_expression(label.condition.kind.clone()) {
                        linear_expressions.push(linear_expression);
                    }
                }
            }
        }
        if !linear_expressions.is_empty() {
            Some(linear_expressions)
        } else { None }
    }

    /// Applies linear programming to the linear expression given, and finds distance in the state to the solution
    fn minimum_distance_1d(&self, state: State, linear_expression: &LinearExpression) -> Option<f32> {
        // Get the declaration from the symbol in LinearExpression, has to be a StateVar
        // (i.e a variable in an LCGS program)
        let symb = self.game.get_decl(&linear_expression.symbol).unwrap();
        if let DeclKind::StateVar(var) = &symb.kind {

            // The range is used for linear programming
            let range_of_var: (f64, f64) = (*var.ir_range.start() as f64, *var.ir_range.end() as f64);

            // Construct the linear programming problem, using minilp rust crate
            // TODO maximize or minimize?
            let mut problem = Problem::new(OptimizationDirection::Minimize);
            let x = problem.add_var(1.0, (range_of_var.0, range_of_var.1));
            // TODO support for more operators?
            match linear_expression.operation {
                Addition => { return None; }
                Multiplication => { return None; }
                Subtraction => { return None; }
                Division => { return None; }
                Equality => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Eq, linear_expression.constant as f64); }
                Inequality => { return None; }
                GreaterThan => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Ge, linear_expression.constant as f64); }
                LessThan => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Le, linear_expression.constant as f64); }
                GreaterOrEqual => { return None; }
                LessOrEqual => { return None; }
                And => { return None; }
                Or => { return None; }
                Xor => { return None; }
                Implication => { return None; }
            }

            match problem.solve() {
                Ok(solution) => {
                    // Now we know that we can in fact solve the linear programming problem, i.e we can satisfy the formula
                    match state.0.get(&linear_expression.symbol) {
                        // The value of our variable in this state we are checking, in "x < 5", this would be "x"
                        Some(&v) => {
                            // Find distance from the current value, to the solution
                            return Some({ f64::abs(v as f64 - solution[x]) } as f32);
                        }
                        _ => { None }
                    }
                }
                Err(..) => {
                    None
                }
            }
        } else { None }
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
                        Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: operator.clone() })
                    } else { return None; }
                } else { return None; }
                // 2nd case
            } else if let ExprKind::OwnedIdent(id) = &operand1.kind {
                if let Identifier::Resolved { owner, name } = *id.clone() {
                    let symbol_of_id = SymbolIdentifier { owner: owner.clone(), name: (name.clone()).parse().unwrap() };
                    if let ExprKind::Number(number) = operand2.kind {
                        Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: operator.clone() })
                    } else { return None; }
                } else { return None; }
            } else { return None; }
        }
        _ => {None}
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