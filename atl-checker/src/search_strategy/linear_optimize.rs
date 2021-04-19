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

struct Point {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct LinearExpression {
    pub x: Identifier,
    pub constant: i32,
    pub operation: BinaryOpKind,
}

/*#[derive(Debug, Default)]
enum LinearExpressionComponent {
    Op(BinaryOpKind),
    Number(i32),
    Variable(Identifier)
}*/

/// Search strategy using ideas from linear programming to order the order in which to visit next
/// vertices, based on distance from the vertex to a region that borders the line between true/false
/// in the formula
pub struct LinearOptimizeSearch {
    queue: VecDeque<Edge<ATLVertex>>,
    game: IntermediateLCGS,
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLCGS) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: VecDeque::new(),
            game,
        }
    }
}

/// A SearchStrategyBuilder for building the LinearOptimizeSearch strategy.
pub struct LinearOptimizeSearchBuilder {
    pub game: IntermediateLCGS
}

impl SearchStrategyBuilder<ATLVertex, LinearOptimizeSearch> for LinearOptimizeSearchBuilder {
    fn build(&self) -> LinearOptimizeSearch {
        LinearOptimizeSearch::new(self.game.clone())
    }
}

impl SearchStrategy<ATLVertex> for LinearOptimizeSearch {
    fn next(&mut self) -> Option<Edge<ATLVertex>> {
        self.queue.pop_front()
    }

    fn queue_new_edges(&mut self, mut edge: Vec<Edge<ATLVertex>>) {
        let mut evaluated_edge: VecDeque<(Edge<ATLVertex>, i32)> = VecDeque::new();

        for edge in edge {
            let distance = self.distance_to_acceptance_border(&edge);

            // Add edge and distance to evaluated edge
            evaluated_edge.push_back((edge, distance))
        };

        // Sort evaluated_edge based on distance
        evaluated_edge.make_contiguous().sort_by_key(|key| key.1);

        // Add all evaluated_edge to queue
        for edge in evaluated_edge.iter() {
            self.queue.push_back(edge.0.clone());
        }
    }
}

impl LinearOptimizeSearch {
    fn distance_to_acceptance_border(&self, edge: &Edge<ATLVertex>) -> i32 {
        let distance = match &edge {
            Edge::HYPER(hyperedge) => {
                // For every target, we want to see how close we get to acceptance border
                let mut distances: Vec<i32> = Vec::new();
                for target in &hyperedge.targets {
                    // Find the linear expression from the targets formula
                    let linear_expression = self.check_if_formula_is_linear(target);

                    // get state of variables
                    let state = self.game.state_from_index(target.state());

                    // get distance from this state, to acceptance
                    if linear_expression.is_some(){
                        let res = minimum_distance(state, linear_expression.unwrap());
                        // add to vec of results
                        distances.push(res)
                    } else {return 100000};


                }
                // TODO remove
                distances.push(3);

                // Find average distance between targets, and return this
                let avg_distance = distances.iter().sum::<i32>() as f32 / distances.len() as f32;
                avg_distance
            }
            // Same procedure for negation edges, just no for loop for all targets, as we only have one target
            Edge::NEGATION(edge) => {
                let linear_expression = self.check_if_formula_is_linear(&edge.target);
                if linear_expression.is_some() {
                    let state = self.game.state_from_index(edge.target.state());
                    minimum_distance(state, linear_expression.unwrap());
                    // TODO remove
                    1.0
                } else {return 100000}
            }
        };
        distance as i32
    }

    fn check_if_formula_is_linear(&self, vertex: &ATLVertex) -> Option<ExprKind> {
        // If outmost Phi is a proposition, return this
        let propositions_index = vertex.formula().get_proposition();

        if propositions_index.is_none() {
            return {None}
        }

        // Check if it was a proposition
        if propositions_index.is_some() {
            // Make sure it is a Label
            if let DeclKind::Label(label) = &self.game.label_index_to_decl(propositions_index.unwrap()).kind {

                // The actual expression of the label (the condition)
                let cond = &label.condition;

                println!("{:?}", cond);

                // Expression has to be linear
                if expression_is_linear(cond) {
                    let expr = &cond.kind;
                    return {Some(expr.clone())}
                } else { return None }
            } else {None}
        } else { return None }
    }
}

//https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
// TODO
fn minimum_distance(state: State, expr: ExprKind) -> i32 {
    3
}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn manhattan_distance(point1: Point, point2: Point) -> i32 {
    i32::abs((point1.x - point2.x) + (point1.y - point2.y))
}

fn expression_is_linear(expr: &Expr) -> bool {
    if let ExprKind::BinaryOp(operator, operand1, operand2) = &expr.kind {
        match operator {
            BinaryOpKind::Division | BinaryOpKind::Multiplication => {
                if let ExprKind::OwnedIdent(id) = &operand1.kind {
                    if operand1 == operand2 {
                        false
                    } else { expression_is_linear(operand1) && expression_is_linear(operand2) }
                } else { expression_is_linear(operand1) && expression_is_linear(operand2) }
            }
            BinaryOpKind::Addition | BinaryOpKind::Subtraction => { expression_is_linear(operand1) && expression_is_linear(operand2) }
            _ => { true }
        }
    } else { true }
}

mod test {
    use crate::search_strategy::linear_optimize::{manhattan_distance, Point, expression_is_linear, LinearExpression};
    use crate::lcgs::ast::{Expr, BinaryOpKind};
    use crate::lcgs::ast::ExprKind::{BinaryOp, OwnedIdent, Number};
    use crate::lcgs::ast::BinaryOpKind::{Equality, Addition, Multiplication};
    use crate::lcgs::ast::Identifier::{Resolved, Simple};
    use crate::lcgs::ast::DeclKind::Player;

    #[test]
    fn manhattan_distance_test() {
        assert_eq!(manhattan_distance(Point { x: 0, y: 0 }, Point { x: 6, y: 6 }), 12);
    }

    #[test]
    fn expression_is_linear_test_two_numbers() {
        let operator = Addition;
        let operand1 = Box::from(Expr { kind: Number(1) });
        let operand2 = Box::from(Expr { kind: Number(1) });
        let expression = Expr { kind: BinaryOp(operator, operand1, operand2) };
        assert_eq!(expression_is_linear(&expression), true)
    }

    #[test]
    fn expression_is_linear_test_simple_linear() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let inner_operand2 = Box::from(Expr { kind: Number(3) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: Number(5) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression_is_linear(&expression), true)
    }

    #[test]
    fn expression_is_linear_test_linear_same_constants_in_mult() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: Number(3) });
        let inner_operand2 = Box::from(Expr { kind: Number(3) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression_is_linear(&expression), true)
    }

    #[test]
    fn expression_is_linear_test_polynomial() {
        let inner_operator = Multiplication;
        let inner_operand1 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });
        let inner_operand2 = Box::from(Expr { kind: OwnedIdent(Box::from(Simple { name: "x".to_string() })) });

        let outer_operator = Addition;
        let outer_operand1 = Box::from(Expr { kind: BinaryOp(inner_operator, inner_operand1, inner_operand2) });
        let outer_operand2 = Box::from(Expr { kind: Number(5) });

        let expression = Expr { kind: BinaryOp(outer_operator, outer_operand1, outer_operand2) };
        assert_eq!(expression_is_linear(&expression), false)
    }

    #[test]
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
        assert_eq!(expression_is_linear(&expression), false)
    }
}

/* TODO
1. Find linear parts of formula
2. Distance function, point - cutting point by a linear equation
3. For each target state, check distance (and if it is in a region)
4. Sort queue by distance (lowest first)
5. ???
6. Profit
 */

/* TODO for reals
1. Skriv extract_single_linear_expression
2. Skriv minimum_distance
*/


/*
notes
hvis f.eks y>3 og y>4, smid y>3 væk og bare behold y>4. Start med at find skæringspunkt, lav normalform, se på højresiden hvilket tal der er størst
Kan tænke over less than or equal.

stop hvis det ikke er muligt
 */

/*
fn extract_single_linear_expression(&self, vertex: &ATLVertex) -> Option<Expr> {
        // If outmost Phi is a proposition, return this
        let propositions_index = vertex.formula().get_proposition();

        // Check if it was a proposition
        if propositions_index.is_some() {
            // Make sure it is a Label
            if let DeclKind::Label(label) = &self.game.label_index_to_decl(propositions_index.unwrap()).kind {

                // The actual expression of the label (the condition)
                let cond = &label.condition;

                println!("{:?}", cond);

                // Expression has to be linear
                if expression_is_linear(cond) {
                    // TODO currently only allows very simple expressions - such as x < 5, y = 1
                    if let ExprKind::BinaryOp(operator, operand1, operand2) = &cond.kind {
                        let variable:Option<Identifier> = match &operand1.kind {
                            ExprKind::OwnedIdent(id) => Some(**id),
                            _ => None
                        };
                        let constant:Option<i32> = match operand2.kind {
                            ExprKind::Number(number) => Some(number),
                            _ => None
                        };
                        let operator: BinaryOpKind = match operator {
                            BinaryOpKind::Addition => Addition,
                            BinaryOpKind::Multiplication => Multiplication,
                            BinaryOpKind::Subtraction => Subtraction,
                            BinaryOpKind::Division => Division,
                            BinaryOpKind::Equality => Equality,
                            BinaryOpKind::Inequality => Inequality,
                            BinaryOpKind::GreaterThan => GreaterThan,
                            BinaryOpKind::LessThan => LessThan,
                            BinaryOpKind::GreaterOrEqual => GreaterOrEqual,
                            BinaryOpKind::LessOrEqual => LessOrEqual,
                            BinaryOpKind::And => And,
                            BinaryOpKind::Or => Or,
                            BinaryOpKind::Xor => Xor,
                            BinaryOpKind::Implication => Implication,
                        };
                        let linearexpression:(Option<Identifier>, Option<i32>, BinaryOpKind) = (variable,constant,operator);
                        return Some(linearexpression)
                    }
                }
            }
        }
        None
    }
 */