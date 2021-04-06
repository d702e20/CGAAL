use crate::common::Edges;
use crate::edg::Vertex;
use crate::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use std::collections::{HashSet, VecDeque};

/// Breadth-first search strategy traverses vertices close to the root first, using a FIFO
/// (first in, first out) data structure.
pub struct BreadthFirstSearch<V: Vertex> {
    queue: VecDeque<Edges<V>>,
}

impl<V: Vertex> BreadthFirstSearch<V> {
    pub fn new() -> BreadthFirstSearch<V> {
        BreadthFirstSearch {
            queue: VecDeque::new(),
        }
    }
}

impl<V: Vertex> SearchStrategy<V> for BreadthFirstSearch<V> {
    fn next(&mut self) -> Option<Edges<V>> {
        self.queue.pop_front()
    }

    fn queue_new_edges(&mut self, edges: HashSet<Edges<V>>) {
        for edge in edges {
            self.queue.push_back(edge);
        }
    }
}

/// A SearchStrategyBuilder for building the BreadthFirstSearch strategy.
pub struct BreadthFirstSearchBuilder;

impl<V: Vertex> SearchStrategyBuilder<V, BreadthFirstSearch<V>> for BreadthFirstSearchBuilder {
    fn build(&self) -> BreadthFirstSearch<V> {
        BreadthFirstSearch::new()
    }
}
