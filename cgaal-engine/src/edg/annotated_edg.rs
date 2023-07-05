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
    pub target: (V, A),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum AnnotatedEdge<V: Hash + Eq + PartialEq + Clone, A: Annotation> {
    Hyper(AnnotatedHyperEdge<V, A>),
    Negation(AnnotatedNegationEdge<V, A>),
}

impl <V: Hash + Eq + PartialEq + Clone, A: Annotation> AnnotatedEdge<V, A> {
    pub fn source(&self) -> &V {
        match self {
            AnnotatedEdge::Hyper(h) => &h.source,
            AnnotatedEdge::Negation(n) => &n.source,
        }
    }

    pub fn annotation(&self) -> Option<&A> {
        match self {
            AnnotatedEdge::Hyper(h) => Some(&h.annotation),
            AnnotatedEdge::Negation(_) => None,
        }
    }

    pub fn targets(&self) -> &[(V, A)] {
        match self {
            AnnotatedEdge::Hyper(h) => &h.targets,
            AnnotatedEdge::Negation(n) => std::slice::from_ref(&n.target),
        }
    }
}