use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::certain_zero::game_strategy::PartialStrategy;
use crate::atl::Phi;
use crate::edg::annotated_edg::{AnnotatedEdge, AnnotatedExtendedDependencyGraph};
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::atledg::AtlDependencyGraph;
use crate::game_structure::{GameStructure, State};
use std::collections::HashMap;

/// Computes the PartialStrategy that the given players can use to enforce the given property.
/// In other words, the behaviour of this function is undefined, if the ATL formula of v0
/// is not an enforce path qualifier or if the formula is not satisfied.
pub fn compute_enforcing_strategy<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    v0: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
) -> PartialStrategy {
    let mut move_to_pick = HashMap::new();

    compute_enforcing_strategy_rec(graph, v0, assignments, &mut move_to_pick);

    PartialStrategy {
        players: v0.formula().players().unwrap().into(),
        move_to_pick,
    }
}

/// Recursive helper function to [compute_enforcing_strategy].
fn compute_enforcing_strategy_rec<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    vertex: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
    move_to_pick: &mut HashMap<State, PartialMove>,
) {
    if let AtlVertex::Full { state, formula } = vertex {
        if move_to_pick.get(state).is_some() {
            // We have already found the move to pick in this state
            return;
        }

        match formula.as_ref() {
            Phi::EnforceNext { .. } => {
                let edges = graph.annotated_succ(vertex);
                // Find the first hyper-edge where all targets are true
                for edge in edges {
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
            Phi::EnforceEventually { .. } => {
                let edges = graph.annotated_succ(vertex);
                // Find the first hyper-edge where all targets are true
                for edge in edges {
                    if let AnnotatedEdge::Hyper(edge) = edge {
                        let all_targets_true = edge.targets.iter().all(|(t, _)| {
                            matches!(assignments.get(t), Some(VertexAssignment::True))
                        });

                        if all_targets_true {
                            // We have found the partial move that will guarantee the property
                            move_to_pick.insert(*state, edge.annotation.unwrap());

                            // Recurse to find the moves of the next states
                            for (target, _) in edge.targets {
                                compute_enforcing_strategy_rec(
                                    graph,
                                    &target,
                                    assignments,
                                    move_to_pick,
                                );
                            }
                        }
                    }
                }
            }
            // Invariant and Until is similar. In both we want to check if the subformula is true
            // in the current state, or otherwise find the first hyper-edge with all true targets.
            Phi::EnforceInvariant { .. } | Phi::EnforceUntil { .. } => {
                let mut edges = graph.annotated_succ(vertex);
                let mut edges_drain = edges.drain(..);

                // Check if subformula is true in this state. We can do this by checking if the
                // target of the first edge is true
                if let Some(AnnotatedEdge::Hyper(edge)) = edges_drain.next() {
                    let all_targets_true = edge
                        .targets
                        .iter()
                        .all(|(t, _)| matches!(assignments.get(t), Some(VertexAssignment::True)));

                    if all_targets_true {
                        // No need for more moves
                        return;
                    }
                }

                // Find the first hyper-edge where all targets are true
                for edge in edges_drain {
                    if let AnnotatedEdge::Hyper(edge) = edge {
                        let all_targets_true = edge.targets.iter().all(|(t, _)| {
                            matches!(assignments.get(t), Some(VertexAssignment::True))
                        });

                        if all_targets_true {
                            // We have found the partial move that will guarantee the property
                            move_to_pick.insert(*state, edge.annotation.unwrap());

                            // Recurse to find the moves of the next states
                            for (target, _) in edge.targets {
                                compute_enforcing_strategy_rec(
                                    graph,
                                    &target,
                                    assignments,
                                    move_to_pick,
                                );
                            }
                        }
                    }
                }
            }
            _ => panic!("Cannot compute enforcing strategy if the formula is not using enforce path qualifier")
        }
    }
}
