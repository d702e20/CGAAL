use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::game_strategy::format::PartialStrategyWithFormatting;
use crate::atl::Phi;
use crate::edg::annotated_edg::{AnnotatedEdge, AnnotatedExtendedDependencyGraph};
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::atledg::AtlDependencyGraph;
use crate::game_structure::{GameStructure, Player, State};
use std::collections::HashMap;

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
    let mut move_to_pick = HashMap::new();

    compute_partial_strategy_rec(graph, v0, assignments, &mut move_to_pick);

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
                        compute_partial_strategy_rec(
                            graph,
                            &edge.target,
                            assignments,
                            move_to_pick,
                        );
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
                            let all_targets_true = edge.targets.iter().all(|(t, _)| {
                                matches!(assignments.get(t), Some(VertexAssignment::True))
                            });

                            if all_targets_true {
                                // We have found the partial move that will guarantee the property
                                move_to_pick.insert(*state, edge.annotation.unwrap());

                                // Recurse to find the moves of the next states
                                for (target, _) in edge.targets {
                                    compute_partial_strategy_rec(
                                        graph,
                                        &target,
                                        assignments,
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
                        compute_partial_strategy_rec(
                            graph,
                            &edge.target,
                            assignments,
                            move_to_pick,
                        );
                    }
                }
                Phi::DespiteEventually { .. } => {
                    let edges = graph.annotated_succ(vertex);
                    // We skip the first edge with the subformula target. We know that it is false.
                    #[cfg(debug_assertions)]
                    {
                        if let AnnotatedEdge::Hyper(e) = edges.get(0).unwrap() {
                            let t = e.targets.get(0).unwrap();
                            let a = assignments.get(&t.0);
                            match a {
                                Some(VertexAssignment::False) => {}
                                _ => panic!(
                                    "First edge of despite was not false contrary to assumption"
                                ),
                            }
                        }
                    }
                    for edge in edges.iter().skip(1) {
                        if let AnnotatedEdge::Hyper(edge) = edge {
                            // We expect to find infinite computations that never hits the `until` formula.
                            // This means there will be a cycle of undecided configurations.
                            // Find the first target assigned undecided.
                            for target in &edge.targets {
                                if matches!(
                                    assignments.get(&target.0),
                                    Some(VertexAssignment::Undecided)
                                ) {
                                    // Target is a partial configuration annotated with the move we need
                                    move_to_pick.insert(*state, target.1.as_ref().unwrap().clone());
                                    compute_partial_strategy_rec(
                                        graph,
                                        &target.0,
                                        assignments,
                                        move_to_pick,
                                    );
                                    return;
                                }
                            }
                        }
                    }
                }
                Phi::DespiteUntil { .. } => {
                    let edges = graph.annotated_succ(vertex);
                    // We skip the first edge with the `until` formula target. We know that it is false.
                    #[cfg(debug_assertions)]
                    {
                        if let AnnotatedEdge::Hyper(e) = edges.get(0).unwrap() {
                            let t = e.targets.get(0).unwrap();
                            let a = assignments.get(&t.0);
                            match a {
                                Some(VertexAssignment::False) => {}
                                _ => panic!(
                                    "First edge of despite was not false contrary to assumption"
                                ),
                            }
                        }
                    }
                    for edge in edges.iter().skip(1) {
                        if let AnnotatedEdge::Hyper(edge) = edge {
                            // If the `pre` formula is also false, then we don't need to check further
                            let pre = edge.targets.get(0).unwrap();
                            if let Some(VertexAssignment::False) = assignments.get(&pre.0) {
                                return;
                            }

                            // We expect to find infinite computations that never hits the `until` formula.
                            // This means there will be a cycle of undecided configurations.
                            // Find the first target assigned undecided.
                            for target in edge.targets.iter().skip(1) {
                                if matches!(
                                    assignments.get(&target.0),
                                    Some(VertexAssignment::Undecided)
                                ) {
                                    // Target is annotated with the move we need
                                    move_to_pick.insert(*state, target.1.as_ref().unwrap().clone());
                                    compute_partial_strategy_rec(
                                        graph,
                                        &target.0,
                                        assignments,
                                        move_to_pick,
                                    );
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
            // The full vertices handles finding the moves so we just need to visit all
            // targets of all the edges to proceed with the computation in the successor states.
            for edge in graph.annotated_succ(vertex) {
                if let AnnotatedEdge::Hyper(edge) = edge {
                    for target in edge.targets {
                        compute_partial_strategy_rec(graph, &target.0, assignments, move_to_pick);
                    }
                }
            }
        }
    }
}
