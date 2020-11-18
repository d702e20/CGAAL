use std::hash::Hash;

pub type WorkerId = u64;

#[derive(Clone, Copy, Debug)]
pub enum VertexAssignment {
    // UNEXPLORED is implemented as hashmap doesn't contain the key/vertex
    UNDECIDED,
    FALSE,
    TRUE,
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
#[derive(Clone)]
pub enum Message<V: Hash + Eq + PartialEq + Clone> {
    /// Send when a hyper-edge is discovered
    HYPER(HyperEdge<V>),
    /// Send when a negation-edge is discovered
    NEGATION(NegationEdge<V>),
    /// Send from a worker that need the final assignment of `vertex` but is not the owner of the vertex.
    REQUEST {
        vertex: V,
        worker_id: WorkerId,
    },
    /// Send from the owner `vertex` to all workers that have request the final assignment of `vertex`
    ANSWER {
        vertex: V,
        assignment: VertexAssignment,
    },
}
