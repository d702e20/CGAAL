use priority_queue::PriorityQueue;
use std::collections::{HashMap, VecDeque};

use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::edg::{Edge, NegationEdge, Vertex};

/// Helper struct for DependencyHeuristicSearch that contains information about a source vertex.
#[derive(Eq, PartialEq, Hash, Default)]
struct DependencyData<V: Vertex> {
    /// The queue edges that have this vertex as the source vertex
    queue: VecDeque<Edge<V>>,
    /// The number of other edges that depends on the assignment of this vertex
    rank: u32,
}

/// Dependency-heuristic search strategy prioritises edges, where the source that has the most
/// dependencies. It is inspired by PageRank and the idea, that the more edges point to the source,
/// the more important the assignment of the source must be.
pub struct DependencyHeuristicSearch<V: Vertex> {
    /// Map from source vertex to its data. A source vertex is not queued unless it has at least
    /// one queue edge
    dependencies: HashMap<V, DependencyData<V>>,
    /// A priority of source vertices that have at least one edge queued
    priority_queue: PriorityQueue<V, u32>,
}

impl<V: Vertex> DependencyHeuristicSearch<V> {
    pub fn new() -> DependencyHeuristicSearch<V> {
        DependencyHeuristicSearch {
            dependencies: Default::default(),
            priority_queue: PriorityQueue::new(),
        }
    }
}

impl<V: Vertex> DependencyHeuristicSearch<V> {
    fn queue(&mut self, edge: Edge<V>) {
        // Queue edge and add source vertex to priority queue
        let dependency = self
            .dependencies
            .entry(edge.source().clone())
            .or_insert(DependencyData {
                queue: Default::default(),
                rank: 0,
            });
        dependency.queue.push_back(edge.clone());
        self.priority_queue
            .push(edge.source().clone(), dependency.rank);
    }
}

impl<V: Vertex> SearchStrategy<V> for DependencyHeuristicSearch<V> {
    fn next(&mut self) -> Option<Edge<V>> {
        self.priority_queue.pop().map(|(source, _)| {
            let data = self.dependencies.get_mut(&source).unwrap();
            let edge = data.queue.pop_front().unwrap();
            if !data.queue.is_empty() {
                // The queue is not empty, so the source is put back into the priority queue
                self.priority_queue.push(source, data.rank);
            }
            edge
        })
    }

    fn queue_new_edges(&mut self, edges: Vec<Edge<V>>) {
        for edge in edges {
            // For each target, increase their priority since we found a new edge,
            // that depends on it
            match &edge {
                Edge::HYPER(edge) => {
                    for target in &edge.targets {
                        let dependency =
                            self.dependencies
                                .entry(target.clone())
                                .or_insert(DependencyData {
                                    queue: Default::default(),
                                    rank: 0,
                                });
                        dependency.rank += 1;
                    }
                }
                Edge::NEGATION(edge) => {
                    let dependency =
                        self.dependencies
                            .entry(edge.target.clone())
                            .or_insert(DependencyData {
                                queue: Default::default(),
                                rank: 0,
                            });
                    dependency.rank += 1;
                }
            }

            self.queue(edge)
        }
    }

    fn queue_released_edges(&mut self, edges: Vec<NegationEdge<V>>) {
        for edge in edges {
            self.queue(Edge::NEGATION(edge));
        }
    }

    fn queue_back_propagation(&mut self, edge: Edge<V>) {
        self.queue(edge);
    }
}

/// A SearchStrategyBuilder for building the DependencyHeuristicSearch strategy.
pub struct DenpdencyHeuristicSearchBuilder;

impl<V: Vertex> SearchStrategyBuilder<V, DependencyHeuristicSearch<V>>
    for DenpdencyHeuristicSearchBuilder
{
    fn build(&self) -> DependencyHeuristicSearch<V> {
        DependencyHeuristicSearch::new()
    }
}
