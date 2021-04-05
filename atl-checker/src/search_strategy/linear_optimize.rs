use crate::common::Edges;
use crate::edg::Vertex;
use crate::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use std::collections::{HashSet, VecDeque, HashMap};
use crate::lcgs::ir::intermediate::IntermediateLCGS;

struct Point {
    x: i32,
    y: i32,
}

/// Search strategy using ideas from linear programming to order the order in which to visit next
/// vertices, based on distance from the vertex to a region that borders the line between true/false
/// in the formula
pub struct LinearOptimizeSearch<V: Vertex> {
    queue: VecDeque<Edges<V>>,
    game: IntermediateLCGS
}

impl<V: Vertex> LinearOptimizeSearch<V> {
    pub fn new(game: IntermediateLCGS) -> LinearOptimizeSearch<V> {
        LinearOptimizeSearch {
            queue: VecDeque::new(),
            game
        }
    }
}

impl<V: Vertex> SearchStrategy<V> for LinearOptimizeSearch<V> {
    fn next(&mut self) -> Option<Edges<V>> {
        self.queue.pop_front()
    }


    /*fn queue_new_edges(&mut self, edges: HashSet<Edges<V>>) {
        for edge in edges {
            self.queue.push_back(edge);
        }
    }*/

    fn queue_new_edges(&mut self, mut edges: HashSet<Edges<V>>) {
        let mut evaluated_edges: VecDeque<(Edges<V>, i32)> = VecDeque::new();

        for edge in edges{
            let average_distance = distance_to_border(&edge);

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

fn distance_to_border<V: Vertex>(edges: &Edges<V>) -> i32 {
    let targets:Vec<V> = match &edges {
        Edges::HYPER(edges) => {
            //println!("{}",edges.targets[0]);
            edges.targets.clone()
        }
        Edges::NEGATION(edges) => {
            vec![edges.target.clone()]
        }
    };

    // find average distance all targets to accept region
    manhattan_distance(Point{x:1,y:1}, Point{x:1,y:1})
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

impl<V: Vertex> SearchStrategyBuilder<V, LinearOptimizeSearch<V>> for LinearOptimizeSearchBuilder {
    fn build(&self) -> LinearOptimizeSearch<V> {
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


/*
for edge in edges{
            let targets:Vec<V> = match &edge {
                Edges::HYPER(edge) => {
                    println!("{}",edge.targets[0]);
                    edge.targets.clone()
                }
                Edges::NEGATION(edge) => {
                    vec![edge.target.clone()]
                }
            };
 */