mod com;
mod multithread;
mod singlethread;

use std::borrow::{Borrow, BorrowMut};
use std::cmp::max;
use crate::edg::{Edge, ExtendedDependencyGraph, HyperEdge, NegationEdge, Vertex};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use crate::algorithms::global::singlethread::SingleThreadedGlobalAlgorithm;

pub trait GlobalAlgorithm<G:ExtendedDependencyGraph<V>, V: Vertex>{
    fn edg(&self) -> &G;
    fn edg_mut(&mut self) -> &mut G;
    fn v_zero(&self) -> &V;
    fn assignment(&self) -> &HashMap<V,bool>;
    fn assignment_mut(&mut self) -> &mut HashMap<V,bool>;

    fn run(&mut self)-> bool;

    /// The resemblance of the function described in the paper, but in order to
    /// know which component we are worker with and the assignments of the component
    /// before, these are both given as arguments. The function it self returns a boolean
    /// which is true if the assignments are changed.
    fn f(&mut self, component: &HashSet<V>) -> bool {
        let mut changed_flag = false;

        for vertex in component {
            for edge in self.edg().succ(vertex) {
                match edge {
                    Edge::Hyper(e) => changed_flag = max(self.process_hyper(e), changed_flag),
                    Edge::Negation(e) => changed_flag = max(self.process_negation(e), changed_flag),
                }
            }
        }
        changed_flag
    }

    /// Rising the value of the source vertex assignment if all targets are true or empty. If the
    /// source vertex already is true we simply return. The return value is based on if a changed
    /// have been made.
    fn process_hyper(&mut self, edge: HyperEdge<V>) -> bool {
        if *self.assignment().get(&edge.source).unwrap() {
            false
        } else {
            let mut final_ass = true;
            for target in edge.targets {
                if !self.assignment().get(&target).unwrap() {
                    final_ass = false;
                    break;
                }
            }
            self.update_assignment(edge.source, final_ass)
        }
    }

    /// Rising the value of the source vertex assignment if the target vertex was assigned
    /// false in the last component assignment. If the source vertex already is true we
    /// simply return. The return value is based on if a changed have been made.
    fn process_negation(&mut self, edge: NegationEdge<V>) -> bool {
        if *self.assignment().get(&edge.source).unwrap() {
            false
        } else {
            let new_ass = !*self.assignment().get(&edge.target).unwrap();
            self.update_assignment(edge.source, new_ass)
        }
    }

    /// Updating an assignment to the new_ass value, if the new_ass are
    /// equal to the old assignment, we simply return. The return value
    /// is based on whether the assignment of the given vertex is changed or not.
    fn update_assignment(&mut self, v: V, new_ass: bool) -> bool {
        self.assignment_mut()
            .get_mut(&v)
            .map(|ass| {
                let old = *ass;
                *ass = new_ass;
                new_ass != old
            })
            .unwrap()
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
                crate::algorithms::global::SingleThreadedGlobalAlgorithm::new($edg_name, $vertex_name::$v).run(),
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
