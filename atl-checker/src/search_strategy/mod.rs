pub mod bfs;

use crate::common::{Edges, NegationEdge};
use crate::edg::Vertex;
use std::collections::HashSet;

pub trait SearchStrategy<V: Vertex> {
    /// Returns the next edge to be processed, or none if there are no safe edges to process.
    fn next(&mut self) -> Option<Edges<V>>;

    /// Queue a set of safe edges with the heuristic
    fn queue_new_edges(&mut self, edges: HashSet<Edges<V>>);

    /// Queue a set of previously unsafe negation edges
    fn queue_released_edges(&mut self, edges: Vec<NegationEdge<V>>) {
        let mut edges = edges;
        self.queue_new_edges(edges.drain(..).map(|edge| Edges::NEGATION(edge)).collect())
    }

    /// Requeue an edge because one of its targets was assigned a certain value
    fn queue_back_propagation(&mut self, edge: Edges<V>) {
        let mut set = HashSet::new();
        set.insert(edge);
        self.queue_new_edges(set)
    }
}

pub trait SearchStrategyBuilder<V: Vertex, S: SearchStrategy<V>> {
    fn build(&self) -> S;
}
