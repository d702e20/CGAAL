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
    Hyper(HyperEdge<V>),
    Negation(NegationEdge<V>),
}

impl <V: Hash + Eq + PartialEq + Clone> Edge<V> {
    /// Returns true if this is a hyper edge
    pub fn is_hyper(&self) -> bool {
        matches!(self, Edge::Hyper(_))
    }

    /// Returns true if this is a negation edge
    pub fn is_negation(&self) -> bool {
        !self.is_hyper()
    }

    /// Returns the source of this edge
    pub fn source(&self) -> &V {
        match self {
            Edge::Hyper(h) => &h.source,
            Edge::Negation(n) => &n.source,
        }
    }

    /// Returns the targets of this edge
    pub fn targets(&self) -> &[V] {
        match self {
            Edge::Hyper(h) => &h.targets,
            Edge::Negation(n) => std::slice::from_ref(&n.target),
        }
    }
}