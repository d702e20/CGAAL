use crate::edg::Vertex;
use std::hash::Hash;

pub trait Annotation: Hash + Eq + PartialEq + Clone {}

pub trait AnnotatedExtendedDependencyGraph<V: Vertex, A: Annotation> {
    /// Returns annotated out-going edges from `vertex`.
    fn annotated_succ(&self, vertex: &V) -> Vec<AnnotatedEdge<V, A>>;
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct AnnotatedHyperEdge<V: Hash + Eq + PartialEq + Clone, A: Annotation> {
    pub source: V,
    pub annotation: A,
    pub targets: Vec<(V, A)>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct AnnotatedNegationEdge<V: Hash + Eq + PartialEq + Clone, A: Annotation> {
    pub source: V,
    pub annotation: A,
    pub target: V,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum AnnotatedEdge<V: Hash + Eq + PartialEq + Clone, A: Annotation> {
    HYPER(AnnotatedHyperEdge<V, A>),
    NEGATION(AnnotatedNegationEdge<V, A>),
}
