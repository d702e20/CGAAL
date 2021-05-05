use crate::algorithms::game_strategy::error::Error;
use std::fmt::{Debug, Display};
use std::hash::Hash;

pub mod annotated_edg;
pub mod atledg;

pub trait Vertex: Hash + Eq + PartialEq + Clone + Display + Debug {}

pub trait ExtendedDependencyGraph<V: Vertex> {
    /// Return out going edges from `vertex`.
    /// This will be cached on each worker.
    fn succ(&self, vertex: &V) -> Vec<Edge<V>>;
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct HyperEdge<V: Hash + Eq + PartialEq + Clone> {
    pub source: V,
    pub targets: Vec<V>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct NegationEdge<V: Hash + Eq + PartialEq + Clone> {
    pub source: V,
    pub target: V,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Edge<V: Hash + Eq + PartialEq + Clone> {
    HYPER(HyperEdge<V>),
    NEGATION(NegationEdge<V>),
}
