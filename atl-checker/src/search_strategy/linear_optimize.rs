use crate::common::Edge;
use crate::edg::Vertex;
use crate::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use std::collections::{HashSet, VecDeque, HashMap};
use crate::lcgs::ir::intermediate::IntermediateLCGS;
use crate::atl::dependencygraph::ATLVertex;
use crate::atl::formula::Phi;
use crate::lcgs::ast::{DeclKind, BinaryOpKind, Expr, ExprKind};
use crate::lcgs::parse::label_decl;

struct Point {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct LinearExpression {
    x: i32,
    constant: i32,
    operation: BinaryOpKind,
}

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
        match &edge {
            Edge::HYPER(hyperedge) => {
                // For every target, we want to see how close we get to acceptance border
                for target in &hyperedge.targets {

                    // Find the propositions we are interested in
                    let linear_expression = self.extract_single_linear_expression(target);
                }
                hyperedge.targets.clone()
            }
            Edge::NEGATION(edge) => {
                let state = self.game.state_from_index(edge.target.state());
                vec![edge.target.clone()]
            }
        };

        // find average distance all targets to accept region
        manhattan_distance(Point { x: 1, y: 1 }, Point { x: 1, y: 1 })
    }

    fn extract_single_linear_expression(&self, vertex: &ATLVertex) -> LinearExpression {
        let propositions_index = vertex.formula().get_proposition();

        if propositions_index.is_some() {
            // Make sure it is a Label
            if let DeclKind::Label(label) = &self.game.label_index_to_decl(propositions_index.unwrap()).kind {

                // The actual expression of the label (the condition)
                let cond = &label.condition;

                if expression_is_linear(cond) {}
            }
        }

        let linear_expression = LinearExpression { x: 1, constant: 2, operation: BinaryOpKind::Addition };
        linear_expression
    }
}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn manhattan_distance(point1: Point, point2: Point) -> i32 {
    i32::abs((point1.x - point2.x) + (point1.y - point2.y))
}

fn expression_is_linear(expr: &Expr) -> bool {
    println!("{:?}", expr);
    if let ExprKind::BinaryOp(operator, operand1, operand2) = &expr.kind {
        match operator {
            BinaryOpKind::Division => {
                //TODO Need more logic here in the future
                if operand1 == operand2 {
                    false
                } else { expression_is_linear(operand1) && expression_is_linear(operand2) }
            }
            BinaryOpKind::Multiplication => {
                //TODO Need more logic here in the future
                if operand1 == operand2 {
                    false
                } else { expression_is_linear(operand1) && expression_is_linear(operand2) }
            }
            BinaryOpKind::Addition => { expression_is_linear(operand1) && expression_is_linear(operand2) }
            BinaryOpKind::Subtraction => { expression_is_linear(operand1) && expression_is_linear(operand2) }
            _ => { true }
        }
    } else { true }
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

mod test {
    use crate::search_strategy::linear_optimize::{manhattan_distance, Point, expression_is_linear};
    use crate::lcgs::ast::Expr;
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
}

/* TODO
1. Find linear parts of formula
2. Distance function, point - cutting point by a linear equation
3. For each target state, check distance (and if it is in a region)
4. Sort queue by distance (lowest first)
5. ???
6. Profit
 */


/*
notes
hvis f.eks y>3 og y>4, smid y>3 væk og bare behold y>4. Start med at find skæringspunkt, lav normalform, se på højresiden hvilket tal der er størst
Kan tænke over less than or equal.

stop hvis det ikke er muligt
 */

