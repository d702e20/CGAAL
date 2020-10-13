pub type Vertex = i32;
pub type WorkerId = i32;

#[derive(Clone, Copy)]
pub enum VertexAssignment {
    UNDECIDED,
    FALSE,
    TRUE,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct HyperEdge {
    pub source: Vertex,
    pub targets: Vec<Vertex>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct NegationEdge {
    pub source: Vertex,
    pub target: Vertex,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Edges {
    HYPER(HyperEdge),
    NEGATION(NegationEdge),
}

#[derive(Clone)]
pub enum Message {
    HYPER(HyperEdge),
    NEGATION(NegationEdge),
    REQUEST { vertex: Vertex, worker_id: WorkerId },
    ANSWER { vertex: Vertex, assignment: VertexAssignment },
    TERMINATE, // NOTE: this is needed along with a separate termination channel to avoid blocking indefinitely on an empty queue
}
