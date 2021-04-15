use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::atl_cgs_edg::{Edge, Vertex};

/// Depth-first search strategy, using a LIFO (last in, first out) data structure.
pub struct DepthFirstSearch<V: Vertex> {
    stack: Vec<Edge<V>>,
}

impl<V: Vertex> DepthFirstSearch<V> {
    pub fn new() -> DepthFirstSearch<V> {
        DepthFirstSearch { stack: Vec::new() }
    }
}

impl<V: Vertex> SearchStrategy<V> for DepthFirstSearch<V> {
    fn next(&mut self) -> Option<Edge<V>> {
        self.stack.pop()
    }

    fn queue_new_edges(&mut self, edges: Vec<Edge<V>>) {
        for edge in edges {
            self.stack.push(edge);
        }
    }
}

/// A SearchStrategyBuilder for building the DepthFirstSearch strategy.
pub struct DepthFirstSearchBuilder;

impl<V: Vertex> SearchStrategyBuilder<V, DepthFirstSearch<V>> for DepthFirstSearchBuilder {
    fn build(&self) -> DepthFirstSearch<V> {
        DepthFirstSearch::new()
    }
}
