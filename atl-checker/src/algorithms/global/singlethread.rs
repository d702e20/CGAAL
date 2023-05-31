use crate::algorithms::global::GlobalAlgorithm;
use crate::edg::{ExtendedDependencyGraph, Vertex};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
// Based on the global algorithm described in "Extended Dependency Graphs and Efficient Distributed Fixed-Point Computation" by A.E. Dalsgaard et al., 2017
pub struct SinglethreadedGlobalAlgorithm<G: ExtendedDependencyGraph<V>, V: Vertex> {
    edg: G,
    /// The root vertex, which we discover the graph from
    v0: V,
    /// The assignment for all vertices, we use this to keep track  of the current assignment
    assignment: HashMap<V, bool>,
    /// The distance of a vertex is represented as the index number of which set the vertex is located in
    dist: VecDeque<HashSet<V>>,
}

impl<
        G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
        V: Vertex + Send + Sync + 'static,
    > GlobalAlgorithm<G, V> for SinglethreadedGlobalAlgorithm<G, V>
{
    fn edg(&self) -> &G {
        &self.edg
    }
    fn edg_mut(&mut self) -> &mut G {
        &mut self.edg
    }
    fn v0(&self) -> &V {
        &self.v0
    }
    fn v0_mut(&mut self) -> &mut V {
        &mut self.v0
    }
    fn assignment(&self) -> &HashMap<V, bool> {
        &self.assignment
    }
    fn assignment_mut(&mut self) -> &mut HashMap<V, bool> {
        &mut self.assignment
    }
    fn dist(&self) -> &VecDeque<HashSet<V>> {
        &self.dist
    }
    fn dist_mut(&mut self) -> &mut VecDeque<HashSet<V>> {
        &mut self.dist
    }
}

impl<
        G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
        V: Vertex + Send + Sync + 'static,
    > SinglethreadedGlobalAlgorithm<G, V>
{
    pub fn run(mut self) -> bool {
        GlobalAlgorithm::<G, V>::run(&mut self)
    }
    pub fn new(edg: G, v0: V) -> Self {
        Self {
            edg,
            v0,
            assignment: HashMap::<V, bool>::new(),
            dist: VecDeque::<HashSet<V>>::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use test_env_log::test;
    #[allow(unused_macros)]
    macro_rules! edg_assert {
        // Standard use, no names or worker count given
        ( $v:ident, $assign:expr ) => {
            edg_assert!([SimpleEDG, SimpleVertex] $v, $assign)
        };
        // With custom names and worker count
        ( [$edg_name:ident, $vertex_name:ident] $v:ident, $assign:expr) => {
            assert_eq!(
                crate::algorithms::global::singlethread::SinglethreadedGlobalAlgorithm::new($edg_name, $vertex_name::$v).run(),
                $assign,
                "Vertex {}",
                stringify!($v)
            );
        };
    }

    #[test]
    fn test_global_algorithm_empty_hyper_edge() {
        simple_edg![
            A => -> {};
        ];
        edg_assert!(A, true);
    }

    #[test]
    fn test_global_algorithm_no_successors() {
        simple_edg![
            A => ;
        ];
        edg_assert!(A, false);
    }

    #[test]
    fn test_global_algorithm_general_01() {
        simple_edg![
            A => -> {B, C} -> {D};
            B => ;
            C => .> D;
            D => -> {};
        ];
        edg_assert!(A, true);
        edg_assert!(B, false);
        edg_assert!(C, false);
        edg_assert!(D, true);
    }

    #[test]
    fn test_global_algorithm_general_02() {
        simple_edg![
            A => -> {B, C};
            B => .> E;
            C => -> {};
            D => -> {} -> {C};
            E => .> D;
        ];
        edg_assert!(A, true);
        edg_assert!(B, true);
        edg_assert!(C, true);
        edg_assert!(D, true);
        edg_assert!(E, false);
    }

    #[test]
    fn test_global_algorithm_general_03() {
        simple_edg![
            A => -> {B} -> {E};
            B => -> {C};
            C => -> {F} -> {H};
            D => -> {E} -> {C};
            E => -> {D, F};
            F => -> {};
            G => .> A;
            H => -> {I};
            I => ;
        ];
        edg_assert!(A, true);
        edg_assert!(B, true);
        edg_assert!(C, true);
        edg_assert!(D, true);
        edg_assert!(E, true);
        edg_assert!(F, true);
        edg_assert!(G, false);
        edg_assert!(H, false);
        edg_assert!(I, false);
    }

    #[test]
    fn test_global_algorithm_general_04() {
        simple_edg![
            A => -> {B} -> {C};
            B => -> {D};
            C => ;
            D => -> {};
        ];
        edg_assert!(A, true);
        edg_assert!(B, true);
        edg_assert!(C, false);
        edg_assert!(D, true);
    }

    #[test]
    fn test_global_algorithm_general_05() {
        simple_edg![
            A => -> {B};
            B => -> {C};
            C => -> {B};
        ];
        edg_assert!(A, false);
        edg_assert!(B, false);
        edg_assert!(C, false);
    }

    #[test]
    fn test_global_algorithm_general_06() {
        simple_edg![
            A => -> {B} -> {C};
            B => ;
            C => ;
        ];
        edg_assert!(A, false);
        edg_assert!(B, false);
        edg_assert!(C, false);
    }

    #[test]
    fn test_global_algorithm_general_07() {
        simple_edg![
            A => -> {B};
            B => -> {A, C};
            C => -> {D};
            D => -> {};
        ];
        edg_assert!(A, false);
        edg_assert!(B, false);
        edg_assert!(C, true);
        edg_assert!(D, true);
    }

    #[test]
    fn test_global_algorithm_general_08() {
        simple_edg![
            A => -> {B, C};
            B => -> {C} -> {D};
            C => -> {B};
            D => -> {C} -> {};
        ];
        edg_assert!(A, true);
        edg_assert!(B, true);
        edg_assert!(C, true);
        edg_assert!(D, true);
    }

    #[test]
    fn test_global_algorithm_negation_01() {
        simple_edg![
            A => .> B;
            B => -> {};
        ];
        edg_assert!(A, false);
        edg_assert!(B, true);
    }

    #[test]
    fn test_global_algorithm_negation_02() {
        simple_edg![
            A => .> B;
            B => -> {C};
            C => -> {B} .> D;
            D => -> {E};
            E => -> {D};
        ];
        edg_assert!(A, false);
        edg_assert!(B, true);
        edg_assert!(C, true);
        edg_assert!(D, false);
        edg_assert!(E, false);
    }

    #[test]
    fn test_global_algorithm_negation_03() {
        simple_edg![
            A => .> B .> C;
            B => .> D;
            C => -> {D};
            D => ;
        ];
        edg_assert!(A, true);
        edg_assert!(B, true);
        edg_assert!(C, false);
        edg_assert!(D, false);
    }

    #[test]
    fn test_global_algorithm_negation_04() {
        simple_edg![
            A => .> B;
            B => -> {B};
        ];
        edg_assert!(A, true);
        edg_assert!(B, false);
    }

    #[test]
    fn test_global_algorithm_negation_05() {
        simple_edg![
            A => .> B;
            B => .> C;
            C => .> D;
            D => .> E;
            E => .> F;
            F => -> {F};
        ];
        edg_assert!(A, true);
        edg_assert!(B, false);
        edg_assert!(C, true);
        edg_assert!(D, false);
        edg_assert!(E, true);
        edg_assert!(F, false);
    }

    #[test]
    fn test_global_algorithm_negation_to_undecided_01() {
        // A case where we might explore and find a negation edges to something that is
        // currently assigned undecided
        simple_edg![
            A => .> B .> E;
            B => -> {C};
            C => -> {D};
            D => .> E;
            E => -> {F};
            F => -> {G};
            G => -> {H};
            H => -> {I};
            I => -> {J};
            J => -> {K};
            K => -> {};
        ];
        edg_assert!(A, true);
        edg_assert!(B, false);
        edg_assert!(C, false);
        edg_assert!(D, false);
        edg_assert!(E, true);
        edg_assert!(F, true);
        edg_assert!(G, true);
    }
}
