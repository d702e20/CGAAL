use crate::common::Edge;
use crate::edg::Vertex;
use crate::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use std::collections::{HashSet, VecDeque, HashMap};
use crate::lcgs::ir::intermediate::{IntermediateLCGS, State};
use crate::atl::dependencygraph::ATLVertex;
use crate::atl::formula::Phi;
use crate::lcgs::ast::{DeclKind, BinaryOpKind, Expr, ExprKind, Identifier};
use crate::lcgs::parse::label_decl;
use crate::lcgs::ast::BinaryOpKind::{Addition, Subtraction, Multiplication, Division, Equality, Inequality, GreaterThan, LessThan, GreaterOrEqual, LessOrEqual, Implication, Xor, Or, And};
use std::env::var;
use minilp;
use minilp::{Problem, OptimizationDirection, ComparisonOp, Error, Solution};
use crate::lcgs::ir::symbol_table::SymbolIdentifier;
use priority_queue::PriorityQueue;
use std::cmp::Reverse;

#[derive(Debug)]
struct LinearExpression {
    pub symbol: SymbolIdentifier,
    pub constant: i32,
    pub operation: BinaryOpKind,
}

/// Search strategy using ideas from linear programming to order the order in which to visit next
/// vertices, based on distance from the vertex to a region that borders the line between true/false
/// in the formula
pub struct LinearOptimizeSearch {
    queue: PriorityQueue<Edge<ATLVertex>, i32>,
    game: IntermediateLCGS,
    sort: i32,
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLCGS) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: PriorityQueue::new(),
            game,
            sort: 0,
        }
    }
}

/// A SearchStrategyBuilder for building the LinearOptimizeSearch strategy.
pub struct LinearOptimizeSearchBuilder {
    pub game: IntermediateLCGS,
}

impl SearchStrategyBuilder<ATLVertex, LinearOptimizeSearch> for LinearOptimizeSearchBuilder {
    fn build(&self) -> LinearOptimizeSearch {
        LinearOptimizeSearch::new(self.game.clone())
    }
}

impl SearchStrategy<ATLVertex> for LinearOptimizeSearch {
    fn next(&mut self) -> Option<Edge<ATLVertex>> {
        let edge = self.queue.pop();
        if edge.is_some() {
            Some(edge.unwrap().0)
        } else { None }
    }

    fn queue_new_edges(&mut self, mut edges: Vec<Edge<ATLVertex>>) {
        // TODO dont do this if formula not linear
        // For all edges from this vertex,
        // if edge is a HyperEdge, return average distance from state to accept region between all targets,
        // if Negation edge, just return the distance from its target
        for edge in edges {
            let distance = self.distance_to_acceptance_border(&edge);

            // Add edge and distance to evaluated_edges
            if let Some(dist) = distance {
                self.queue.push(edge, -dist as i32);
            } else {
                // Todo default value should be average?
                self.queue.push(edge, 0);
            }
        };
    }
}

impl LinearOptimizeSearch {
    fn distance_to_acceptance_border(&self, edge: &Edge<ATLVertex>) -> Option<f32> {
        match &edge {
            Edge::HYPER(hyperedge) => {
                // For every target of the hyperedge, we want to see how close we are to acceptance border
                let mut distances: Vec<f32> = Vec::new();
                for target in &hyperedge.targets {
                    // TODO only allows very simple expressions, such as x < 5, should allow more
                    // TODO change edge by changing order of targets in the edge, based on distance
                    // Find the linear expression from the targets formula, if any
                    // Polynomials and such not allowed, returns None in such cases
                    let linear_expression = self.get_simple_linear_formula(target);

                    if let Some(expr) = linear_expression {
                        // get the State in the target
                        let state = self.game.state_from_index(target.state());

                        // Distance from the state, to fulfilling the linear expression
                        let distance = self.minimum_distance_1d(state, expr);

                        // add to vec of results
                        if let Some(dist) = distance {
                            distances.push(dist)
                        }
                    } else {
                        return None;
                    }
                }

                // If no targets were able to satisfy formula, or something went wrong, return large number
                if { distances.is_empty() } {
                    return None;
                }

                // Find average distance between targets, and return this
                let avg_distance = (distances.iter().sum::<f32>() / distances.len() as f32);
                return Some(avg_distance);
            }
            // Same procedure for negation edges, just no for loop for all targets, as we only have one target
            Edge::NEGATION(edge) => {
                let linear_expression = self.get_simple_linear_formula(&edge.target);
                if let Some(expr) = linear_expression {
                    let state = self.game.state_from_index(edge.target.state());
                    let distance = self.minimum_distance_1d(state, expr);
                    distance
                } else {
                    None
                }
            }
        }
    }

    fn get_simple_linear_formula(&self, vertex: &ATLVertex) -> Option<LinearExpression> {
        // If outmost Phi is a proposition, get this
        let mut proposition_index = vertex.formula().get_proposition();

        // It could also be that we have "F Phi" and then we recursively visit formula to find propositions
        // Currently it is MVP and if there is exactly one formula, return this
        if proposition_index.is_none() {
            let propositions = vertex.formula().get_propositions_recursively();
            if propositions.len() == 1 {
                proposition_index = propositions.get(0).cloned();
            }
        }
        // Check if it was a proposition
        if let Some(propositions_index) = proposition_index {
            // Make sure it is a Label
            if let DeclKind::Label(label) = &self.game.label_index_to_decl(propositions_index).kind {
                // Expression has to be linear
                if label.condition.is_linear() {
                    // Return the constructed Linear Expression from this condition
                    return extracted_linear_expression(label.condition.kind.clone());
                }
            }
        }
        None
    }


    fn minimum_distance_1d(&self, state: State, lin_expr: LinearExpression) -> Option<f32> {
        // Get the declaration from the symbol in LinearExpression, has to be a StateVar
        // (i.e a variable in an LCGS program)
        let symb = self.game.get_decl(&lin_expr.symbol).unwrap();
        if let DeclKind::StateVar(var) = &symb.kind {

            // The range is used for linear programming
            let range_of_var: (f64, f64) = (*var.ir_range.start() as f64, *var.ir_range.end() as f64);

            // Construct the linear programming problem, using minilp rust crate
            // TODO maximize or minimize?
            let mut problem = Problem::new(OptimizationDirection::Maximize);
            let x = problem.add_var(1.0, (range_of_var.0, range_of_var.1));
            // TODO support for more operators?
            match lin_expr.operation {
                Addition => { return None; }
                Multiplication => { return None; }
                Subtraction => { return None; }
                Division => { return None; }
                Equality => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Eq, lin_expr.constant as f64); }
                Inequality => { return None; }
                GreaterThan => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Ge, lin_expr.constant as f64); }
                LessThan => { problem.add_constraint(&[(x, 1.0)], ComparisonOp::Le, lin_expr.constant as f64); }
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
                    match state.0.get(&lin_expr.symbol) {
                        // The value of our variable in this state we are checking, in "x < 5", this would be "x"
                        Some(&v) => {
                            // Find distance from the current value, to the solution
                            return Some({ f64::abs((v as f64 - solution[x])) } as f32);
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

fn extracted_linear_expression(expr: ExprKind) -> Option<LinearExpression> {
    match &expr {
        ExprKind::BinaryOp(operator, operand1, operand2) => {
            if let ExprKind::OwnedIdent(id) = &operand1.kind {
                if let Identifier::Resolved { owner, name } = *id.clone() {
                    let symbol_of_id = SymbolIdentifier { owner: owner.clone(), name: (name.clone()).parse().unwrap() };
                    if let ExprKind::Number(number) = operand2.kind {
                        match operator {
                            Addition => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Addition }) }
                            Multiplication => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Multiplication }) }
                            Subtraction => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Subtraction }) }
                            Division => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Division }) }
                            Equality => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Equality }) }
                            Inequality => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Inequality }) }
                            GreaterThan => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: GreaterThan }) }
                            LessThan => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: LessThan }) }
                            GreaterOrEqual => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: GreaterOrEqual }) }
                            LessOrEqual => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: LessOrEqual }) }
                            And => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: And }) }
                            Or => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Or }) }
                            Xor => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Xor }) }
                            Implication => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Implication }) }
                        }
                    } else { return None; }
                } else { return None; }
                // 2nd case
            } else if let ExprKind::OwnedIdent(id) = &operand1.kind {
                if let Identifier::Resolved { owner, name } = *id.clone() {
                    let symbol_of_id = SymbolIdentifier { owner: owner.clone(), name: (name.clone()).parse().unwrap() };
                    if let ExprKind::Number(number) = operand2.kind {
                        match operator {
                            Addition => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Addition }) }
                            Multiplication => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Multiplication }) }
                            Subtraction => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Subtraction }) }
                            Division => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Division }) }
                            Equality => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Equality }) }
                            Inequality => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Inequality }) }
                            GreaterThan => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: GreaterThan }) }
                            LessThan => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: LessThan }) }
                            GreaterOrEqual => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: GreaterOrEqual }) }
                            LessOrEqual => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: LessOrEqual }) }
                            And => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: And }) }
                            Or => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Or }) }
                            Xor => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Xor }) }
                            Implication => { Some(LinearExpression { symbol: symbol_of_id, constant: number, operation: Implication }) }
                        }
                    } else { return None; }
                } else { return None; }
            } else { return None; }
        }
        _ => return None
    }
}

mod test {
    use crate::search_strategy::linear_optimize::{LinearOptimizeSearch, LinearExpression};
    use crate::lcgs::ast::{Expr, BinaryOpKind};
    use crate::lcgs::ast::ExprKind::{BinaryOp, OwnedIdent, Number};
    use crate::lcgs::ast::BinaryOpKind::{Equality, Addition, Multiplication};
    use crate::lcgs::ast::Identifier::{Resolved, Simple};
    use crate::lcgs::ast::DeclKind::Player;
    use crate::lcgs::ir::intermediate::{State, IntermediateLCGS};
    use crate::lcgs::ir::symbol_table::{SymbolIdentifier, Owner};
    use std::collections::HashMap;
    use crate::lcgs::parse::parse_lcgs;
    use crate::atl::gamestructure::GameStructure;

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
        let operand1 = Box::from(Expr { kind: Number(1) });
        let operand2 = Box::from(Expr { kind: Number(1) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression.is_linear(), false)
    }

    #[test]
    // x + x
    fn expression_is_linear_test_two_variables1() {
        let operator = Addition;
        let operand1 = Box::from(Expr { kind: Number(1) });
        let operand2 = Box::from(Expr { kind: Number(1) });
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

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), lin_exp);
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

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), lin_exp);
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

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), lin_exp);
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

        let solution = LinearOptimizeSearch::new(lcgs.clone()).minimum_distance_1d(initial.clone(), lin_exp);
        assert!(solution.is_none());
    }
}