use crate::common::Edges;
use crate::edg::{ExtendedDependencyGraph, Vertex};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

/// The SolveSetAssignment is assigned to vertices of an EDG when using the minimum_solve_set
/// algorithm. A solve set contains the vertices that need to be check to find the certain
/// assignment of the vertex (including itself) given a local algorithm with a perfect heuristic.
/// Note that the true/false part of the assignment is not guaranteed to match the
/// minimum fixed-point assignment of the certain_zero algorithm.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SolveSetAssignment<V: Hash + Eq + PartialEq + Clone + Debug> {
    /// BeingCalculated indicate that the assignment is currently unknown but that vertex
    /// have been discovered. This value is used for vertices further up in the stack.
    /// If the algorithm needs the assignment of a vertex and finds the BeingCalculated, then
    /// it must have hit a cycle in the EDG.
    BeingCalculated,

    /// True indicate that the vertex can be assigned certain 1 by checking the given set
    /// of vertices, assuming a local algorithm with a perfect heuristic.
    True(HashSet<V>),

    /// False indicate that the vertex can be assigned certain 0 by checking the given set
    /// of vertices, assuming a local algorithm with a perfect heuristic.
    /// The boolean indicates if the assignment is uncertain (depends on a cycle)
    False(HashSet<V>, bool),
}

impl<V: Hash + Eq + PartialEq + Clone + Debug> SolveSetAssignment<V> {
    /// Get the number of elements in the solve set. Will panic if not calculated.
    pub fn len(&self) -> usize {
        match self {
            SolveSetAssignment::BeingCalculated => panic!("Solve set is being calculated"),
            SolveSetAssignment::True(vertices) => vertices.len(),
            SolveSetAssignment::False(vertices, _) => vertices.len(),
        }
    }

    /// Get the number of elements in the solve set, negative if SolveSetAssignment::False.
    /// The sign is an implementation detail, so this function is mainly for debugging.
    #[allow(dead_code)]
    pub fn signed_len(&self) -> i32 {
        match self {
            SolveSetAssignment::BeingCalculated => panic!("Solve set is being calculated"),
            SolveSetAssignment::True(vertices) => vertices.len() as i32,
            SolveSetAssignment::False(vertices, _) => -(vertices.len() as i32),
        }
    }

    /// Returns true if this assignment is uncertain (depends on a cycle).
    /// Will panic if not calculated.
    pub fn is_uncertain(&self) -> bool {
        match self {
            SolveSetAssignment::BeingCalculated => panic!("Solve set is being calculated"),
            SolveSetAssignment::False(_, true) => true,
            _ => false,
        }
    }
}

/// The minimum_solve_set algorithm will find the smallest set of vertices that need to be checked
/// to find the assigment of every vertex in an extended dependency graph assuming a local
/// algorithm with a perfect heuristic. The algorithm uses a recursive approach
/// since a perfect heuristic would induce a depth-first search that only explores the vertices
/// needed to confirm the root's assigment.
/// Note that found solve sets are minimum, but not necessarily the only minimum solve set.
/// Also, note that the true/false part of the assignment of a vertex is not guaranteed to match the
/// minimum fixed-point assignment of the certain_zero algorithm. Only the root is guaranteed
/// to be correct. The true/false part is used in the algorithm to find short circuiting.
/// Lastly, some vertices will be given an uncertain false assignment. This means their assignment
/// depends on a cycle and how it is entered. These assignments are therefore only as correct as
/// the root requires them to be. They could be bigger in some cases, but this should be good
/// enough for any analysis, since their sizes relative to sibling edges, are correct.
pub fn minimum_solve_set<G: ExtendedDependencyGraph<V>, V: Vertex>(
    edg: &G,
    root: V,
) -> HashMap<V, SolveSetAssignment<V>> {
    let mut assignments = HashMap::new();
    find_solve_set_rec(edg, root, &mut assignments);
    assignments
}

/// Recursive part of minimum_solve_set algorithm. Here we find the solve set of the given
/// vertex by exploring all its edges.
fn find_solve_set_rec<G: ExtendedDependencyGraph<V>, V: Vertex>(
    edg: &G,
    vertex: V,
    assignments: &mut HashMap<V, SolveSetAssignment<V>>,
) -> SolveSetAssignment<V> {
    // We might know the assignment already, otherwise, calculate it.
    // We also recalculate it, if it is uncertain (part of cycle)
    let prev_assignment = assignments.get(&vertex).cloned();
    if matches!(
        prev_assignment,
        None | Some(SolveSetAssignment::False(_, true))
    ) {
        // Mark this vertex as being calculated to find cycles
        assignments.insert(vertex.clone(), SolveSetAssignment::BeingCalculated);

        let edges = edg.succ(&vertex);

        // We can short circuit when we find one edge, which assigns this vertex to true.
        // Hence, we hope to find the edges that makes the assignment true with the fewest vertices,
        // hence we keep track of the best (smallest) of the true-edges. If no such true-edge exist,
        // then we have to evaluate all false-edges to confirm it. So we union the false-sets.
        let mut best_true_set: Option<HashSet<V>> = None;
        let mut false_union: HashSet<V> = HashSet::new();
        // If any of the false-edges are uncertain, this will be set true
        let mut any_uncertain = false;

        for edge in edges {
            match edge {
                Edges::HYPER(hyper) => {
                    // In the world of hyper-edges, we can short circuit when we find one edge,
                    // where the target is assigned false. So this is the inverse of before:
                    // We hope to find the edges that makes the assignment false with the fewest
                    // vertices, hence we keep track of the best (smallest) of the false-edges.
                    // If no such false-edge exist, then we have to evaluate all true-edges to
                    // confirm it. So we union the true-sets.
                    let mut best_false_set: Option<HashSet<V>> = None;
                    let mut true_union: HashSet<V> = HashSet::new();
                    let mut best_is_uncertain = false;

                    for target in hyper.targets {
                        match find_solve_set_rec(edg, target, assignments) {
                            SolveSetAssignment::BeingCalculated => {
                                // This is a cycle. This is essentially false in Ø.
                                best_false_set = Some(HashSet::new());
                                best_is_uncertain = true;
                            }
                            SolveSetAssignment::False(vertices, uncertain) => {
                                if !matches!(best_false_set, Some(ref best) if best.len() < vertices.len())
                                {
                                    // We found a (smaller) false set
                                    best_false_set = Some(vertices.clone());
                                    best_is_uncertain = uncertain;
                                }
                            }
                            SolveSetAssignment::True(vertices) => {
                                true_union = true_union.union(&vertices).cloned().collect()
                            }
                        }
                    }

                    if let Some(vertices) = best_false_set {
                        // We found at least one false target, so this hyper-edge is a false-edge
                        false_union = false_union.union(&vertices).cloned().collect();
                        any_uncertain = any_uncertain || best_is_uncertain
                    } else {
                        // We found no false targets, so we check if this is the best true-edge
                        if !matches!(best_true_set, Some(ref best) if best.len() < true_union.len())
                        {
                            best_true_set = Some(true_union);
                        }
                    }
                }
                Edges::NEGATION(negation) => {
                    // When target is true, source if false, and vice versa. But the set of
                    // vertices that need to be checked remain the same
                    match find_solve_set_rec(edg, negation.target, assignments) {
                        SolveSetAssignment::BeingCalculated => panic!("EDG is not negation safe!"),
                        SolveSetAssignment::True(vertices) => {
                            false_union = false_union.union(&vertices).cloned().collect()
                        }
                        SolveSetAssignment::False(vertices, _) => {
                            if !matches!(best_true_set, Some(ref best) if best.len() < vertices.len())
                            {
                                // We found a (smaller) true set
                                best_true_set = Some(vertices.clone());
                            }
                        }
                    }
                }
            }
        }

        let solve_set = if let Some(mut vertices) = best_true_set {
            // We found at least one true-edge, so this vertex is true
            // Also add self
            vertices.insert(vertex.clone());
            SolveSetAssignment::True(vertices)
        } else {
            // We found no true-edges, so this vertex is false
            // Also add self
            false_union.insert(vertex.clone());
            SolveSetAssignment::False(false_union, any_uncertain)
        };

        // If we are recalculating an uncertain assignment, check if the new one is worse
        if solve_set.is_uncertain() && prev_assignment.is_some() {
            let prev_assign = prev_assignment.unwrap();
            debug_assert!(
                prev_assign.is_uncertain(),
                "We should not be recalculating certain answers"
            );
            if solve_set.len() > prev_assign.len() {
                assignments.insert(vertex.clone(), solve_set.clone());
            } else {
                assignments.insert(vertex.clone(), prev_assign);
            }
        } else {
            // No uncertainty in this solve set, just update
            assignments.insert(vertex.clone(), solve_set.clone());
        }

        solve_set
    } else {
        prev_assignment.unwrap().clone()
    }
}

#[cfg(test)]
mod test {
    use crate::solve_set::minimum_solve_set;
    /// Defines a test of the solve distance algorithm.
    /// Meant to be used in conjunction with `simple_edg`.
    ///
    /// # Example
    /// ```
    /// assert_solve_dist!(
    ///     A => 2,
    ///     B => 1,
    /// )
    /// ```
    macro_rules! assert_signed_solve_set_lens {
        // Standard use, no custom names
        ( root=$root:ident, $( $v:ident => $size:expr, )* ) => {
            assert_signed_solve_set_lens!([SimpleEDG, SimpleVertex] root=$root, $( $v => $size, )* )
        };
        // With custom names given
        ( [$edg_name:ident, $vertex_name:ident] root=$root:ident, $( $v:ident => $size:expr, )* ) => {
            let dists = minimum_solve_set(&$edg_name, $vertex_name::$root);
            $( assert_eq!(dists.get(&$vertex_name::$v).unwrap().signed_len(), $size, stringify!($v)); )*
        };
    }

    #[test]
    fn test_mss_basic_01() {
        // No successors
        simple_edg![
            A => -> {};
            B => ;
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 1,
        );
        assert_signed_solve_set_lens!(
            root=B,
            B => -1,
        );
    }

    #[test]
    fn test_mss_basic_02() {
        // Hyper-edge, all targets negative
        simple_edg![
            A => -> {B, C};
            B => ;
            C => -> {B};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => -2,
            B => -1,
            C => -2,
        );
    }

    #[test]
    fn test_mss_basic_03() {
        // Hyper-edge, one target negative
        simple_edg![
            A => -> {B, C};
            B => -> {};
            C => ;
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => -2,
            B => 1,
            C => -1,
        );
    }

    #[test]
    fn test_mss_basic_04() {
        // Hyper-edge, all targets positive
        simple_edg![
            A => -> {B, C};
            B => -> {};
            C => -> {D};
            D => -> {};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 4, // B:1 + C:2 + 1 = 4
            B => 1,
            C => 2,
            D => 1,
        );
    }

    #[test]
    fn test_mss_basic_05() {
        // Cycle
        simple_edg![
            A => -> {B};
            B => -> {C};
            C => -> {A};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => -3,
            B => -2,
            C => -1,
        );
    }

    #[test]
    fn test_mss_basic_06() {
        // Multiple hyper-edges, all targets negative
        simple_edg![
            A => -> {B} -> {C};
            B => ;
            C => -> {D};
            D => ;
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => -4, // B:-1 + C:-2 - 1 = -4
            B => -1,
            C => -2,
            D => -1,
        );
    }

    #[test]
    fn test_mss_basic_07() {
        // Multiple hyper-edges, one target negative
        simple_edg![
            A => -> {B} -> {C};
            B => -> {};
            C => ;
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 2,
            B => 1,
            C => -1,
        );
    }

    #[test]
    fn test_mss_basic_08() {
        // Multiple hyper-edges, all targets positive
        simple_edg![
            A => -> {B} -> {C};
            B => -> {};
            C => -> {B};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 2,
            B => 1,
            C => 2,
        );
    }

    #[test]
    fn test_mss_basic_09() {
        // Hyper-edge vs negation edge, both negative
        simple_edg![
            A => -> {B} .> C;
            B => ;
            C => ;
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 2,
            B => -1,
            C => -1,
        );
    }

    #[test]
    fn test_mss_basic_10() {
        // Hyper-edge vs negation edge, both positive
        simple_edg![
            A => -> {B} .> C;
            B => -> {};
            C => -> {};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 2,
            B => 1,
            C => 1,
        );
    }

    #[test]
    fn test_mss_basic_11() {
        // Negative hyper-edge vs positive negation edge
        simple_edg![
            A => -> {B} .> C;
            B => ;
            C => -> {};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => -3,
            B => -1,
            C => 1,
        );
    }

    #[test]
    fn test_mss_basic_12() {
        // Positive hyper-edge vs negative negation edge
        simple_edg![
            A => -> {B} .> C;
            B => -> {};
            C => ;
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 2,
            B => 1,
            C => -1,
        );
    }

    #[test]
    fn test_mss_avoid_double_counting_01() {
        // Hyper-edge targets share dependencies, but we should only count each once
        simple_edg![
            A => -> {B,C,D};
            B => -> {};
            C => -> {B};
            D => -> {C};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 4,
            B => 1,
            C => 2,
            D => 3,
        );
    }

    #[test]
    fn test_mss_avoid_double_counting_02() {
        // Hyper-edge targets share dependencies, but we should only count each once
        simple_edg![
            A => -> {B,C};
            B => -> {D,E};
            C => -> {D,E};
            D => -> {};
            E => -> {};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 5,
            B => 3,
            C => 3,
            D => 1,
            E => 1,
        );
    }

    #[test]
    fn test_mss_uncertainty_01() {
        // Vertex C has to be recalculated due to how we enter the cycle the first time
        simple_edg![
            A => -> {B} -> {C};
            B => -> {D};
            C => -> {B};
            D => -> {C} -> {};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 3,
            B => 2,
            C => 3,
            D => 1,
        );
    }

    #[test]
    fn test_mss_uncertainty_02() {
        // Vertex B is updated to worse uncertainty if A->B is checked second
        simple_edg![
            A => -> {B} -> {C};
            B => -> {D};
            C => -> {D} -> {};
            D => -> {B,C};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 2,
            B => -2,
            C => 1,
            D => -1,
        );
    }

    #[test]
    fn test_mss_uncertainty_03() {
        // Vertex B and C is NOT recalculated since it is stray
        simple_edg![
            A => -> {B} -> {};
            B => -> {C};
            C => -> {A};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 1,
            B => -2,
            C => -1,
        );
    }

    #[test]
    fn test_mss_uncertainty_04() {
        // Vertex E is recalculated, however, vertex D is NOT recalculated since it is stray,
        // which means sometimes D is -2 other times -1
        simple_edg![
            A => -> {B} -> {E};
            B => -> {C};
            C => -> {} -> {D};
            D => -> {E};
            E => -> {B};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => 3,
            B => 2,
            C => 1,
            E => 3,
        );
    }

    #[test]
    fn test_mss_uncertainty_05() {
        // Vertex C and E are recalculated to worse uncertain false
        simple_edg![
            A => -> {C} -> {E};
            B => -> {C};
            C => -> {D};
            D => -> {E};
            E => -> {B};
        ];
        assert_signed_solve_set_lens!(
            root=A,
            A => -5,
            B => -3,
            C => -4,
            D => -3,
            E => -4,
        );
    }
}
