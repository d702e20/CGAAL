use crate::common::Edges;
use crate::edg::Vertex;
use crate::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use std::collections::{HashSet, VecDeque};
use crate::lcgs::ir::intermediate::IntermediateLCGS;

/// Breadth-first search strategy traverses vertices close to the root first, using a FIFO
/// (first in, first out) data structure.
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

    fn queue_new_edges(&mut self, edges: HashSet<Edges<V>>) {
        for edge in edges {
            self.queue.push_back(edge);

        }
    }

}

fn manhattan_distance() -> f32{
    32.0
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
    use crate::search_strategy::linear_optimize::{manhattan_distance};

    #[test]
    fn linear_test() {
        assert_eq!(manhattan_distance(), 32.0);
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