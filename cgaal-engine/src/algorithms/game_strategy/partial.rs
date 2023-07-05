use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::game_strategy::format::PartialStrategyWithFormatting;
use crate::atl::Phi;
use crate::edg::annotated_edg::{AnnotatedEdge, AnnotatedExtendedDependencyGraph};
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::atledg::AtlDependencyGraph;
use crate::game_structure::{GameStructure, Player, State};
use std::collections::{HashMap, HashSet};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct PartialStrategy {
    /// The players for which this strategy applies
    pub players: Vec<Player>,
    /// A partial mapping from States to a partial move, where the given players have made
    /// a specific choice.
    pub move_to_pick: HashMap<State, PartialMove>,
}

impl PartialStrategy {
    /// Pairs a partial strategy with its game structure, allowing us to print
    /// the strategy using the names of players and moves as defined by the game structure.
    pub fn in_context_of<'a, G: GameStructure>(
        &'a self,
        game: &'a G,
    ) -> PartialStrategyWithFormatting<'a, G> {
        PartialStrategyWithFormatting {
            strategy: self,
            game,
        }
    }
}

/// Computes the PartialStrategy that the given players can use to enforce or break
/// the given property. In other words, the behaviour of this function is undefined, if the
/// ATL formula of v0 is an unsatisfied enforce property or a satisfied despite property.
pub fn compute_partial_strategy<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    v0: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
) -> PartialStrategy {
    let mut visited = HashSet::new();
    let mut move_to_pick = HashMap::new();

    compute_partial_strategy_rec(graph, v0, assignments, &mut visited, &mut move_to_pick);

    PartialStrategy {
        players: v0.formula().players().unwrap().into(),
        move_to_pick,
    }
}

/// Recursive helper function to [compute_partial_strategy].
fn compute_partial_strategy_rec<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    vertex: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
    visited: &mut HashSet<State>,
    move_to_pick: &mut HashMap<State, PartialMove>,
) {
    visited.insert(vertex.state());
    compute_partial_strategy_rec_inner(graph, vertex, assignments, visited, move_to_pick);
    visited.remove(&vertex.state());
}

/// Recursive helper function to [compute_partial_strategy].
fn compute_partial_strategy_rec_inner<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    vertex: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
    visited: &mut HashSet<State>,
    move_to_pick: &mut HashMap<State, PartialMove>,
) {
    match vertex {
        AtlVertex::Full { state, formula } => {
            if move_to_pick.get(state).is_some() {
                // We have already found the move to pick in this state
                return;
            }

            match formula.as_ref() {
                Phi::EnforceNext { .. } => {
                    // Find the first hyper-edge where all targets are true
                    for edge in graph.annotated_succ(vertex) {
                        if let AnnotatedEdge::Hyper(edge) = edge {
                            let all_targets_true = edge.targets.iter().all(|(t, _)| {
                                matches!(assignments.get(t), Some(VertexAssignment::True))
                            });

                            if all_targets_true {
                                // We have found the partial move that will guarantee the property
                                move_to_pick.insert(*state, edge.annotation.unwrap());
                                return;
                            }
                        }
                    }
                }
                Phi::EnforceInvariant { .. } => {
                    // Invariants belong to the maximum fixed point domain, so it only has one
                    // negation edge to an DespiteUntil. Let's visit that one instead.
                    let edges = graph.annotated_succ(vertex);
                    if let Some(AnnotatedEdge::Negation(edge)) = edges.get(0) {
                        compute_partial_strategy_rec_inner(
                            graph,
                            &edge.target.0,
                            assignments,
                            visited,
                            move_to_pick,
                        );
                    } else {
                        unreachable!("Invariant formulae has exactly one negation edge")
                    }
                }
                Phi::EnforceEventually { .. } | Phi::EnforceUntil { .. } => {
                    let mut edges = graph.annotated_succ(vertex);
                    let mut edges_drain = edges.drain(..);

                    // Check if subformula is true in this state. We can do this by checking if the
                    // target of the first edge is true
                    if let Some(AnnotatedEdge::Hyper(edge)) = edges_drain.next() {
                        let all_targets_true = edge.targets.iter().all(|(t, _)| {
                            matches!(assignments.get(t), Some(VertexAssignment::True))
                        });

                        if all_targets_true {
                            // No need for more moves
                            return;
                        }
                    }

                    // Find the first hyper-edge where all targets are true
                    for edge in edges_drain {
                        if let AnnotatedEdge::Hyper(edge) = edge {
                            let all_targets_true_and_unvisited = edge.targets.iter().all(|(t, _)| {
                                matches!(assignments.get(t), Some(VertexAssignment::True)) && !visited.contains(&t.state())
                            });

                            if all_targets_true_and_unvisited {
                                // We have found the partial move that will guarantee the property
                                move_to_pick.insert(*state, edge.annotation.unwrap());

                                // Recurse to find the moves of the next states
                                for (target, _) in edge.targets {
                                    compute_partial_strategy_rec(
                                        graph,
                                        &target,
                                        assignments,
                                        visited,
                                        move_to_pick,
                                    );
                                }
                                return;
                            }
                        }
                    }
                }
                Phi::DespiteNext { .. } => {
                    // There is only one hyper-edge
                    for edge in graph.annotated_succ(vertex) {
                        if let AnnotatedEdge::Hyper(edge) = edge {
                            // Find the first target assigned false
                            for target in edge.targets {
                                if matches!(
                                    assignments.get(&target.0),
                                    Some(VertexAssignment::False)
                                ) {
                                    // Target is annotated with the move we need
                                    move_to_pick.insert(*state, target.1.unwrap());
                                    return;
                                }
                            }
                        }
                    }
                }
                Phi::DespiteInvariant { .. } => {
                    // Invariants belong to the maximum fixed point domain, so it only has one
                    // negation edge to an EnforceUntil. Let's visit that one instead.
                    let edges = graph.annotated_succ(vertex);
                    if let Some(AnnotatedEdge::Negation(edge)) = edges.get(0) {
                        compute_partial_strategy_rec_inner(
                            graph,
                            &edge.target.0,
                            assignments,
                            visited,
                            move_to_pick,
                        );
                    } else {
                        unreachable!("Invariant formulae has exactly one negation edge")
                    }
                }
                Phi::DespiteEventually { .. } => {
                    // Given that we are looking for a strategy for in this vertex, it must be
                    // the case that the coalition can ensure that the subformula never holds.
                    // Hence, this vertex is assigned undecided and we are looking for a
                    // cycle of undecided DespiteEventually vertices.

                    let edges = graph.annotated_succ(vertex);

                    // We skip the first edge with the subformula target. We know that it is false.
                    debug_assert_eq!(assignments.get(&edges[0].targets()[0].0), Some(VertexAssignment::False).as_ref());

                    for edge in edges.iter().skip(1) {
                        if let AnnotatedEdge::Hyper(edge) = edge {
                            // Find the first target assigned undecided.
                            for target in &edge.targets {
                                if matches!(
                                    assignments.get(&target.0),
                                    Some(VertexAssignment::Undecided)
                                ) {
                                    move_to_pick.insert(*state, target.1.as_ref().unwrap().clone());
                                    compute_partial_strategy_rec(
                                        graph,
                                        &target.0,
                                        assignments,
                                        visited,
                                        move_to_pick,
                                    );
                                    return;
                                }
                            }
                        }
                    }
                }
                Phi::DespiteUntil { .. } => {
                    // Given that we are looking for a strategy for in this vertex, it must be
                    // the case that the coalition can ensure that either
                    // - the until-subformula never holds, or
                    // - eventually neither the pre-subformula and the until-subformula holds.
                    // In the former case, this vertex is assigned undecided and we are looking
                    // for a cycle of undecided DespiteUntil vertices. In the latter case,
                    // this vertex is assigned false, and we are looking for a path/branch of
                    // false DespiteUntil vertices.

                    let edges = graph.annotated_succ(vertex);

                    // We skip the first edge with the subformula target. We know that it is false.
                    debug_assert_eq!(assignments.get(&edges[0].targets()[0].0), Some(VertexAssignment::False).as_ref());

                    if matches!(assignments.get(vertex), Some(VertexAssignment::Undecided)) {
                        for edge in edges.iter().skip(1) {
                            // Skip the pre-subformula and find the first target assigned undecided.
                            for target in edge.targets().iter().skip(1) {
                                if matches!(
                                    assignments.get(&target.0),
                                    Some(VertexAssignment::Undecided)
                                ) {
                                    move_to_pick.insert(*state, target.1.as_ref().unwrap().clone());
                                    compute_partial_strategy_rec(
                                        graph,
                                        &target.0,
                                        assignments,
                                        visited,
                                        move_to_pick,
                                    );
                                    return;
                                }
                            }
                        }
                    } else {
                        for edge in edges.iter().skip(1) {
                            // Either the pre-subformula is false, or there exists a move such
                            // that the DespiteUntil formula is false at all possible destinations.
                            // Find the first target assigned false.
                            't: for target in edge.targets() {
                                if matches!(
                                    assignments.get(&target.0),
                                    Some(VertexAssignment::False)
                                ) {
                                    if target.0.formula().is_despite() {
                                        // There may be cycle back to the DespiteUntil vertex.
                                        // We prevent this by ensuring the target has no visited
                                        // successors.
                                        for edge in graph.annotated_succ(&target.0) {
                                            for target in edge.targets() {
                                                if visited.contains(&target.0.state()) {
                                                    continue 't;
                                                }
                                            }
                                        }
                                        move_to_pick.insert(*state, target.1.as_ref().unwrap().clone());
                                        compute_partial_strategy_rec(
                                            graph,
                                            &target.0,
                                            assignments,
                                            visited,
                                            move_to_pick,
                                        );
                                    }
                                    return;
                                }
                            }
                        }
                    }
                }
                _ => panic!("The formula does not require a strategy"),
            }
        }
        AtlVertex::Partial { .. } => {
            // Partial vertices are only used by despite formulae.
            // If this vertex is undecided, then we just need to find a undecided child.
            // If this vertex is false, then we need to visit all children.
            if matches!(assignments.get(&vertex), Some(VertexAssignment::Undecided)) {
                for edge in graph.annotated_succ(vertex) {
                    for target in edge.targets() {
                        if matches!(assignments.get(&target.0), Some(VertexAssignment::Undecided)) {
                            compute_partial_strategy_rec_inner(
                                graph,
                                &target.0,
                                assignments,
                                visited,
                                move_to_pick,
                            );
                            return;
                        }
                    }
                }
            } else {
                for edge in graph.annotated_succ(vertex) {
                    for target in edge.targets() {
                        compute_partial_strategy_rec_inner(graph, &target.0, assignments, visited, move_to_pick);
                    }
                }
            }
        }
    }
}
