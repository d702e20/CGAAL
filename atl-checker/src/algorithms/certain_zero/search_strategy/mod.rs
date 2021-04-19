use crate::edg::{Edge, NegationEdge, Vertex};

pub mod bfs;
pub mod dependency_heuristic;
pub mod dfs;

/// A SearchStrategy defines in which order safe edges of an EDG is processed first in the
/// certain zero algorithm.
/// The trait supports multiple functions to queue edges. This allows us to make search strategies
/// that prioritises certain reasons queueing differently.
pub trait SearchStrategy<V: Vertex> {
    /// Returns the next edge to be processed, or none if there are no safe edges to process.
    fn next(&mut self) -> Option<Edge<V>>;

    /// Queue a set of safe edges with the heuristic
    fn queue_new_edges(&mut self, edges: Vec<Edge<V>>);

    /// Queue a set of previously unsafe negation edges
    fn queue_released_edges(&mut self, edges: Vec<NegationEdge<V>>) {
        let mut edges = edges;
        self.queue_new_edges(edges.drain(..).map(Edge::NEGATION).collect())
    }

    /// Requeue an edge because one of its targets was assigned a certain value
    fn queue_back_propagation(&mut self, edge: Edge<V>) {
        self.queue_new_edges(vec![edge])
    }
}

/// A SearchStrategyBuilder is able to create an instance of a SearchStrategy.
/// This trait allows us to create a SearchStrategy instance for each worker in the certain zero
/// algorithm, based on the settings given by the user.
pub trait SearchStrategyBuilder<V: Vertex, S: SearchStrategy<V>> {
    fn build(&self) -> S;
}
