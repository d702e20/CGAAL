use std::collections::VecDeque;

use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::atl_cgs_edg::{Edge, Vertex};

/// Breadth-first search strategy traverses vertices close to the root first, using a FIFO
/// (first in, first out) data structure.
pub struct BreadthFirstSearch<V: Vertex> {
    queue: VecDeque<Edge<V>>,
}

impl<V: Vertex> BreadthFirstSearch<V> {
    pub fn new() -> BreadthFirstSearch<V> {
        BreadthFirstSearch {
            queue: VecDeque::new(),
        }
    }
}

impl<V: Vertex> SearchStrategy<V> for BreadthFirstSearch<V> {
    fn next(&mut self) -> Option<Edge<V>> {
        self.queue.pop_front()
    }

    fn queue_new_edges(&mut self, edges: Vec<Edge<V>>) {
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
