use std::hash::Hash;

pub type WorkerId = u64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

#[derive(Clone)]
pub enum Message<V: Hash + Eq + PartialEq + Clone> {
    HYPER(HyperEdge<V>),
    NEGATION(NegationEdge<V>),
    REQUEST {
        vertex: V,
        worker_id: WorkerId,
    },
    ANSWER {
        vertex: V,
        assignment: VertexAssignment,
    },
}
