use crate::atl::atl_cgs_edg::Edge;
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

impl VertexAssignment {
    /// Returns true if the assignment is either true or false.
    pub fn is_certain(self) -> bool {
        return matches!(self, VertexAssignment::TRUE | VertexAssignment::FALSE);
    }
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

impl<V: Hash + Eq + PartialEq + Clone> Edge<V> {
    /// Returns true if this is a hyper edge
    pub fn is_hyper(&self) -> bool {
        matches!(self, Edge::HYPER(_))
    }

    /// Returns true if this is a negation edge
    pub fn is_negation(&self) -> bool {
        !self.is_hyper()
    }

    /// Returns the source vertex of this edge
    pub fn source(&self) -> &V {
        match self {
            Edge::HYPER(e) => &e.source,
            Edge::NEGATION(e) => &e.source,
        }
    }

    /// Returns the targets vertices of this edge
    pub fn targets(&self) -> Vec<&V> {
        match self {
            Edge::HYPER(e) => e.targets.iter().collect(),
            Edge::NEGATION(e) => vec![&e.target],
        }
    }
}

/// Inter-Worker communication
#[derive(Clone, Debug)]
pub enum Message<V: Hash + Eq + PartialEq + Clone> {
    /// Send from a worker that needs the final assignment of `vertex` but is not the owner of the vertex.
    REQUEST {
        vertex: V,
        depth: u32,
        worker_id: WorkerId,
    },
    /// Send from the owner of `vertex` to all workers that have requested the final assignment of `vertex`
    ANSWER {
        vertex: V,
        assignment: VertexAssignment,
    },
    TOKEN(MsgToken),
    /// Release component/negation-edges of `depth` depth
    RELEASE(usize),
    /// Terminate the worker
    TERMINATE,
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
