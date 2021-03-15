use crate::common::Edges;
use crate::edg::{ExtendedDependencyGraph, Vertex};
use std::collections::HashMap;

/// The SolveDistAssignment is assigned to vertices of an EDG when using the solve_dist algorithm.
/// The solve distance is how many iterations are require to find the certain assignment of a
/// the vertex given a local algorithm with a perfect heuristic.
/// Note that the true/false part of the assignment is not guaranteed to match the
/// minimum fixed-point assignment of the certain_zero algorithm.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum SolveDistAssignment {
    /// BeingCalculated indicate that the assignment is currently unknown and that vertex
    /// have been discovered. This value is used for vertices further up in the stack.
    /// If the algorithm needs the assignment of a vertex assignment BeingCalculated, then
    /// it must have hit a cycle in the EDG.   
    BeingCalculated,

    /// True indicate that the vertex can be assigned certain 1 in the given amount
    /// of iterations, assuming a local algorithm with a perfect heuristic.
    True(u32),

    /// False indicate that the vertex can be assigned certain 0 in the given amount
    /// of iterations, assuming a local algorithm with a perfect heuristic.
    False(u32),
}

impl SolveDistAssignment {
    /// Get the solve distance. Will panic if not calculated.
    pub fn dist(self) -> u32 {
        match self {
            SolveDistAssignment::BeingCalculated => panic!("Solve distance is being calculated."),
            SolveDistAssignment::True(dist) => dist,
            SolveDistAssignment::False(dist) => dist,
        }
    }

    /// Get the signed solve distance. The sign is an implementation detail, so this function
    /// is mainly for debugging.
    pub fn signed_dist(self) -> i32 {
        match self {
            SolveDistAssignment::BeingCalculated => panic!("Solve distance is being calculated."),
            SolveDistAssignment::True(dist) => dist as i32,
            SolveDistAssignment::False(dist) => -(dist as i32),
        }
    }
}

/// The solve_dist algorithm will find the number of iterations needed to find the assigment
/// of every vertex in an extended dependency graph assuming a local algorithm with a perfect
/// heuristic. This is the so-called solve distance. The algorithm uses a recursive approach
/// since a perfect heuristic would induce a depth-first search that only explores the vertices
/// needed to confirm the root's assigment.
/// Note that the true/false part of the assignment of a vertex is not guaranteed to match the
/// minimum fixed-point assignment of the certain_zero algorithm. Only the root is guaranteed
/// to be correct. The true/false part is used in the algorithm to find short circuiting.
pub fn solve_dist<G: ExtendedDependencyGraph<V>, V: Vertex>(
    edg: &G,
    root: V,
) -> HashMap<V, SolveDistAssignment> {
    let mut assignments = HashMap::new();
    find_solve_dist(edg, root, &mut assignments);
    assignments
}

/// Recursive part of solve_dist algorithm. Here we find the solve distance of the given
/// vertex by exploring all its edges.
fn find_solve_dist<G: ExtendedDependencyGraph<V>, V: Vertex>(
    edg: &G,
    vertex: V,
    assignments: &mut HashMap<V, SolveDistAssignment>,
) -> SolveDistAssignment {
    // We might know the assignment already, otherwise, calculate it
    assignments.get(&vertex).copied().unwrap_or_else(move || {
        // Mark this vertex as being calculated to find cycles
        assignments.insert(vertex.clone(), SolveDistAssignment::BeingCalculated);

        let edges = edg.succ(&vertex);

        // We can short circuit when we find one edge, which assigns this vertex to true.
        // Hence, we hope to find the edges that makes the assignment true with the least distance,
        // so we keep track of the best of the true-edges. If no such true-edge exist, then
        // we have to evaluate all (false-)edges to confirm it. So we sum that false-distance.
        let mut best_true_dist: Option<u32> = None;
        let mut summed_false_dist = 0;

        for edge in edges {
            match edge {
                Edges::HYPER(hyper) => {
                    // In the world of hyper-edges, we can short circuit when we find one edge,
                    // where the target is assigned false. So this is the inverse of before:
                    // We hope to find the edges that makes the assignment false with the least distance,
                    // so we keep track of the best of the true-edges. If no such false-edge exist, then
                    // we have to evaluate all (true-)edges to confirm it. So we sum that true-distance.
                    let mut best_false_dist: Option<u32> = None;
                    let mut summed_true_dist = 0;

                    for target in hyper.targets {
                        match find_solve_dist(edg, target, assignments) {
                            SolveDistAssignment::BeingCalculated => {
                                // This is a cycle. This indicates false in 0.
                                best_false_dist = Some(0);
                            }
                            SolveDistAssignment::False(dist) => {
                                if !matches!(best_false_dist, Some(best) if best < dist) {
                                    best_false_dist = Some(dist);
                                }
                            }
                            SolveDistAssignment::True(dist) => summed_true_dist += dist,
                        }
                    }

                    if let Some(dist) = best_false_dist {
                        // We found at least one false target, so this is a false-edge
                        summed_false_dist += dist;
                    } else {
                        // We found no false targets, so we check if the best true-edge so far
                        if !matches!(best_true_dist, Some(best) if best < summed_true_dist) {
                            best_true_dist = Some(summed_true_dist);
                        }
                    }
                }
                Edges::NEGATION(negation) => {
                    // Note: When target is true, source if false, and vice versa
                    match find_solve_dist(edg, negation.target, assignments) {
                        SolveDistAssignment::BeingCalculated => panic!("EDG is not negation safe!"),
                        SolveDistAssignment::True(dist) => summed_false_dist += dist,
                        SolveDistAssignment::False(dist) => {
                            if !matches!(best_true_dist, Some(best) if best < dist) {
                                best_true_dist = Some(dist);
                            }
                        }
                    }
                }
            }
        }

        let solve_dist = if let Some(dist) = best_true_dist {
            // We found at least one true-edge, so this vertex is true
            SolveDistAssignment::True(dist + 1) // +1 to include self
        } else {
            // We found no true-edges, so this vertex is false
            SolveDistAssignment::False(summed_false_dist + 1) // +1 to include self
        };

        // Save result and return
        assignments.insert(vertex, solve_dist);
        solve_dist
    })
}

#[cfg(test)]
mod test {
    use crate::solve_dist::solve_dist;
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
    macro_rules! assert_signed_solve_dists {
        // Standard use, no custom names
        ( root=$root:ident, $( $v:ident => $sd:expr, )* ) => {
            assert_signed_solve_dists!([SimpleEDG, SimpleVertex] root=$root, $( $v => $sd, )* )
        };
        // With custom names given
        ( [$edg_name:ident, $vertex_name:ident] root=$root:ident, $( $v:ident => $sd:expr, )* ) => {
            let dists = solve_dist(&$edg_name, $vertex_name::$root);
            $( assert_eq!(dists.get(&$vertex_name::$v).unwrap().signed_dist(), $sd); )*
        };
    }

    #[test]
    fn test_sd_basic_01() {
        // No successors
        simple_edg![
            A => -> {};
            B => ;
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 1,
        );
        assert_signed_solve_dists!(
            root=B,
            B => -1,
        );
    }

    #[test]
    fn test_sd_basic_02() {
        // Hyper-edge, all targets negative
        simple_edg![
            A => -> {B, C};
            B => ;
            C => -> {B};
        ];
        assert_signed_solve_dists!(
            root=A,
            A => -2,
            B => -1,
            C => -2,
        );
    }

    #[test]
    fn test_sd_basic_03() {
        // Hyper-edge, one target negative
        simple_edg![
            A => -> {B, C};
            B => -> {};
            C => ;
        ];
        assert_signed_solve_dists!(
            root=A,
            A => -2,
            B => 1,
            C => -1,
        );
    }

    #[test]
    fn test_sd_basic_04() {
        // Hyper-edge, all targets positive
        simple_edg![
            A => -> {B, C};
            B => -> {};
            C => -> {D};
            D => -> {};
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 4, // B:1 + C:2 + 1 = 4
            B => 1,
            C => 2,
            D => 1,
        );
    }

    #[test]
    fn test_sd_basic_05() {
        // Cycle
        simple_edg![
            A => -> {B};
            B => -> {C};
            C => -> {A};
        ];
        assert_signed_solve_dists!(
            root=A,
            A => -3,
            B => -2,
            C => -1,
        );
    }

    #[test]
    fn test_sd_basic_06() {
        // Multiple hyper-edges, all targets negative
        simple_edg![
            A => -> {B} -> {C};
            B => ;
            C => -> {D};
            D => ;
        ];
        assert_signed_solve_dists!(
            root=A,
            A => -4, // B:-1 + C:-2 - 1 = -4
            B => -1,
            C => -2,
            D => -1,
        );
    }

    #[test]
    fn test_sd_basic_07() {
        // Multiple hyper-edges, one target negative
        simple_edg![
            A => -> {B} -> {C};
            B => -> {};
            C => ;
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 2,
            B => 1,
            C => -1,
        );
    }

    #[test]
    fn test_sd_basic_08() {
        // Multiple hyper-edges, all targets positive
        simple_edg![
            A => -> {B} -> {C};
            B => -> {};
            C => -> {B};
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 2,
            B => 1,
            C => 2,
        );
    }

    #[test]
    fn test_sd_basic_09() {
        // Hyper-edge vs negation edge, both negative
        simple_edg![
            A => -> {B} .> C;
            B => ;
            C => ;
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 2,
            B => -1,
            C => -1,
        );
    }

    #[test]
    fn test_sd_basic_10() {
        // Hyper-edge vs negation edge, both positive
        simple_edg![
            A => -> {B} .> C;
            B => -> {};
            C => -> {};
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 2,
            B => 1,
            C => 1,
        );
    }

    #[test]
    fn test_sd_basic_11() {
        // Negative hyper-edge vs positive negation edge
        simple_edg![
            A => -> {B} .> C;
            B => ;
            C => -> {};
        ];
        assert_signed_solve_dists!(
            root=A,
            A => -3,
            B => -1,
            C => 1,
        );
    }

    #[test]
    fn test_sd_basic_12() {
        // Positive hyper-edge vs negative negation edge
        simple_edg![
            A => -> {B} .> C;
            B => -> {};
            C => ;
        ];
        assert_signed_solve_dists!(
            root=A,
            A => 2,
            B => 1,
            C => -1,
        );
    }
}
