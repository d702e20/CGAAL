use crate::distterm::Weight;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

pub type WorkerId = u64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VertexAssignment {
    // UNEXPLORED is implemented as hashmap doesn't contain the key/vertex
    UNDECIDED,
    FALSE,
    TRUE,
}

impl Display for VertexAssignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VertexAssignment::UNDECIDED => write!(f, "undecided"),
            VertexAssignment::FALSE => write!(f, "false"),
            VertexAssignment::TRUE => write!(f, "true"),
        }
    }
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
pub enum Edges<V: Hash + Eq + PartialEq + Clone> {
    HYPER(HyperEdge<V>),
    NEGATION(NegationEdge<V>),
}

/// Inter-Worker communication
#[derive(Clone, Debug)]
pub enum Message<V: Hash + Eq + PartialEq + Clone> {
    /// Send from a worker that needs the final assignment of `vertex` but is not the owner of the vertex.
    REQUEST {
        vertex: V,
        distance: u32,
        worker_id: WorkerId,
        weight: Weight,
    },
    /// Send from the owner of `vertex` to all workers that have requested the final assignment of `vertex`
    ANSWER {
        vertex: V,
        assignment: VertexAssignment,
        weight: Weight,
    },
}
