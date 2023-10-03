use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::edg::{Edge, Vertex};

/// Random depth-first search (RDFS) strategy inspired by TAPAAL's search strategy of the same name.
/// The strategy is depth-first search, but with a random ordering of the edges.
pub struct RandomDepthFirstSearch<V: Vertex> {
    stack: Vec<Edge<V>>,
}

impl<V: Vertex> RandomDepthFirstSearch<V> {
    pub fn new() -> RandomDepthFirstSearch<V> {
        RandomDepthFirstSearch { stack: Vec::new() }
    }
}

impl<V: Vertex> Default for RandomDepthFirstSearch<V> {
    fn default() -> Self {
        RandomDepthFirstSearch::new()
    }
}

impl<V: Vertex> SearchStrategy<V> for RandomDepthFirstSearch<V> {
    fn next(&mut self) -> Option<Edge<V>> {
        self.stack.pop()
    }

    fn queue_new_edges(&mut self, edges: Vec<Edge<V>>) {
        // A new batch of edges to be queued are first shuffled, then pushed to stack.
        // Efficient shuffling since Edge impls hash and by exploiting salted hashmap.
        let unique_edges: std::collections::HashSet<_> = edges.into_iter().collect();

        for edge in unique_edges {
            self.stack.push(edge);
        }
    }
}

/// A SearchStrategyBuilder for building the RandomDepthFirstSearch strategy.
pub struct RandomDepthFirstSearchBuilder;

impl<V: Vertex> SearchStrategyBuilder<V, RandomDepthFirstSearch<V>>
    for RandomDepthFirstSearchBuilder
{
    fn build(&self, _root: &V) -> RandomDepthFirstSearch<V> {
        RandomDepthFirstSearch::new()
    }
}
