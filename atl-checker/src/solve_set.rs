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
    False(HashSet<V>),
}

impl<V: Hash + Eq + PartialEq + Clone + Debug> SolveSetAssignment<V> {
    /// Get the number of elements in the solve set. Will panic if not calculated.
    pub fn len(&self) -> usize {
        match self {
            SolveSetAssignment::BeingCalculated => panic!("Solve distance is being calculated."),
            SolveSetAssignment::True(vertices) => vertices.len(),
            SolveSetAssignment::False(vertices) => vertices.len(),
        }
    }

    /// Get the number of elements in the solve set, negative if SolveSetAssignment::False.
    /// The sign is an implementation detail, so this function is mainly for debugging.
    pub fn signed_len(&self) -> i32 {
        match self {
            SolveSetAssignment::BeingCalculated => panic!("Solve distance is being calculated."),
            SolveSetAssignment::True(vertices) => vertices.len() as i32,
            SolveSetAssignment::False(vertices) => -(vertices.len() as i32),
        }
    }
}

/// The minimum_solve_set algorithm will find the smallest set of vertices that need to be checked
/// to find the assigment of every vertex in an extended dependency graph assuming a local
/// algorithm with a perfect heuristic. The algorithm uses a recursive approach
/// since a perfect heuristic would induce a depth-first search that only explores the vertices
/// needed to confirm the root's assigment.
/// Note that found solve sets are minimum, but not necessarily unique.
/// Also, note that the true/false part of the assignment of a vertex is not guaranteed to match the
/// minimum fixed-point assignment of the certain_zero algorithm. Only the root is guaranteed
/// to be correct. The true/false part is used in the algorithm to find short circuiting.
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
    // We might know the assignment already, otherwise, calculate it
    assignments.get(&vertex).cloned().unwrap_or_else(move || {
        // Mark this vertex as being calculated to find cycles
        assignments.insert(vertex.clone(), SolveSetAssignment::BeingCalculated);

        let edges = edg.succ(&vertex);

        // We can short circuit when we find one edge, which assigns this vertex to true.
        // Hence, we hope to find the edges that makes the assignment true with the fewest vertices,
        // hence we keep track of the best (smallest) of the true-edges. If no such true-edge exist,
        // then we have to evaluate all false-edges to confirm it. So we union the false-sets.
        let mut best_true_set: Option<HashSet<V>> = None;
        let mut false_union: HashSet<V> = HashSet::new();

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

                    for target in hyper.targets {
                        match find_solve_set_rec(edg, target, assignments) {
                            SolveSetAssignment::BeingCalculated => {
                                // This is a cycle. This is essentially false in Ø.
                                best_false_set = Some(HashSet::new());
                            }
                            SolveSetAssignment::False(vertices) => {
                                if !matches!(best_false_set, Some(ref best) if best.len() < vertices.len()) {
                                    // We found a (smaller) false set
                                    best_false_set = Some(vertices.clone());
                                }
                            }
                            SolveSetAssignment::True(vertices) => {
                                true_union = true_union.union(&vertices).cloned().collect()
                            },
                        }
                    }

                    if let Some(vertices) = best_false_set {
                        // We found at least one false target, so this hyper-edge is a false-edge
                        false_union = false_union.union(&vertices).cloned().collect();
                    } else {
                        // We found no false targets, so we check if this is the best true-edge
                        if !matches!(best_true_set, Some(ref best) if best.len() < true_union.len()) {
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
                        SolveSetAssignment::False(vertices) => {
                            if !matches!(best_true_set, Some(ref best) if best.len() < vertices.len()) {
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
            SolveSetAssignment::False(false_union)
        };

        // Save result and return
        assignments.insert(vertex.clone(), solve_set.clone());
        solve_set
    })
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
            $( assert_eq!(dists.get(&$vertex_name::$v).unwrap().signed_len(), $size); )*
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
}
