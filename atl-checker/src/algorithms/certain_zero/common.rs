use crate::edg::Edge;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

pub type WorkerId = u64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VertexAssignment {
    // UNEXPLORED is implemented as hashmap doesn't contain the key/vertex
    Undecided,
    False,
    True,
}

impl VertexAssignment {
    /// Returns true if the assignment is either true or false.
    pub fn is_certain(self) -> bool {
        return matches!(self, VertexAssignment::True | VertexAssignment::False);
    }
}

impl Display for VertexAssignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VertexAssignment::Undecided => write!(f, "undecided"),
            VertexAssignment::False => write!(f, "false"),
            VertexAssignment::True => write!(f, "true"),
        }
    }
}

impl<V: Hash + Eq + PartialEq + Clone> Edge<V> {
    /// Returns true if this is a hyper edge
    pub fn is_hyper(&self) -> bool {
        matches!(self, Edge::Hyper(_))
    }

    /// Returns true if this is a negation edge
    pub fn is_negation(&self) -> bool {
        !self.is_hyper()
    }

    /// Returns the source vertex of this edge
    pub fn source(&self) -> &V {
        match self {
            Edge::Hyper(e) => &e.source,
            Edge::Negation(e) => &e.source,
        }
    }

    /// Returns the targets vertices of this edge
    pub fn targets(&self) -> Vec<&V> {
        match self {
            Edge::Hyper(e) => e.targets.iter().collect(),
            Edge::Negation(e) => vec![&e.target],
        }
    }
}

/// Inter-Worker communication
#[derive(Clone, Debug)]
pub enum Message<V: Hash + Eq + PartialEq + Clone> {
    /// Send from a worker that needs the final assignment of `vertex` but is not the owner of the vertex.
    Request {
        vertex: V,
        depth: u32,
        worker_id: WorkerId,
    },
    /// Send from the owner of `vertex` to all workers that have requested the final assignment of `vertex`
    Answer {
        vertex: V,
        assignment: VertexAssignment,
    },
    Token(MsgToken),
    /// Release component/negation-edges of `depth` depth
    Release(usize),
    /// Terminate the worker
    Terminate,
}

#[derive(Clone, Debug, PartialOrd, Ord, Eq, PartialEq)]
pub enum Token {
    /// Indicate that no previous holder of the token have any pending hyper- or negations-edges
    Clean = 0,
    /// Indicate that no previous holder of the token have pending hyper-edges, but at least one do have pending negation-edges
    HaveNegations = 1,
    /// Indicate that a previous holder of the token have pending hyper-edges
    Dirty = 2,
}

#[derive(Clone, Debug)]
pub struct MsgToken {
    pub(crate) token: Token,
    pub(crate) deepest_component: usize,
}