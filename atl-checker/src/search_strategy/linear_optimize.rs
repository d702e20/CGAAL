use crate::common::Edges;
use crate::edg::Vertex;
use crate::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use std::collections::{HashSet, VecDeque, HashMap};
use crate::lcgs::ir::intermediate::IntermediateLCGS;
use crate::atl::dependencygraph::ATLVertex;

struct Point {
    x: i32,
    y: i32,
}

/// Search strategy using ideas from linear programming to order the order in which to visit next
/// vertices, based on distance from the vertex to a region that borders the line between true/false
/// in the formula
pub struct LinearOptimizeSearch {
    queue: VecDeque<Edges<ATLVertex>>,
    game: IntermediateLCGS
}

impl LinearOptimizeSearch {
    pub fn new(game: IntermediateLCGS) -> LinearOptimizeSearch {
        LinearOptimizeSearch {
            queue: VecDeque::new(),
            game
        }
    }
}

impl SearchStrategy<ATLVertex> for LinearOptimizeSearch {
    fn next(&mut self) -> Option<Edges<ATLVertex>> {
        self.queue.pop_front()
    }

    fn queue_new_edges(&mut self, mut edges: HashSet<Edges<ATLVertex>>) {
        let mut evaluated_edges: VecDeque<(Edges<ATLVertex>, i32)> = VecDeque::new();

        for edge in edges{
            let average_distance = self.distance_to_acceptance_border(&edge);

            // add edge and distance to evaluated edges
            evaluated_edges.push_back((edge, average_distance))
        };
        // Sort evaluated_edges based on distance
        evaluated_edges.make_contiguous().sort_by_key(|key| key.1);

        // Add all evaluated_edges to queue
        for edge in evaluated_edges.iter() {
            self.queue.push_back(edge.0.clone());
        }
    }
}

impl LinearOptimizeSearch{
    fn distance_to_acceptance_border(&self, edges: &Edges<ATLVertex>) -> i32 {
        match &edges {
            Edges::HYPER(edges) => {
                let state = edges.targets[0].state();
                println!("{}", state_vars[0]);
                edges.targets.clone()
            }
            Edges::NEGATION(edges) => {
                vec![edges.target.clone()]
            }
        };

        // find average distance all targets to accept region
        manhattan_distance(Point{x:1,y:1}, Point{x:1,y:1})
    }
}


fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn manhattan_distance(point1: Point, point2: Point) -> i32{
    i32::abs((point1.x - point2.x) + (point1.y - point2.y))
}

/// A SearchStrategyBuilder for building the LinearOptimizeSearch strategy.
pub struct LinearOptimizeSearchBuilder{
    pub game: IntermediateLCGS
}

impl SearchStrategyBuilder<ATLVertex, LinearOptimizeSearch> for LinearOptimizeSearchBuilder {
    fn build(&self) -> LinearOptimizeSearch {
        LinearOptimizeSearch::new(self.game.clone())
    }
}

mod test {
    use crate::search_strategy::linear_optimize::{manhattan_distance, Point};

    #[test]
    fn linear_test() {
        assert_eq!(manhattan_distance(Point { x: 0, y: 0 }, Point { x: 6, y: 6 }), 12);
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
