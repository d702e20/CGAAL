use std::collections::{HashMap, HashSet};

use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::game_strategy::format::PartialStrategyWithFormatting;
use crate::edg::annotated_edg::AnnotatedExtendedDependencyGraph;
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::atledg::AtlDependencyGraph;
use crate::edg::ExtendedDependencyGraph;
use crate::game_structure::{GameStructure, PlayerIdx, StateIdx};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct PartialStrategy {
    /// The players for which this strategy applies
    pub players: Vec<PlayerIdx>,
    /// A partial mapping from States to a partial move, where the given players have made
    /// a specific choice.
    pub move_to_pick: HashMap<StateIdx, PartialMove>,
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

/// Compute the moves of the witness strategy.
/// This function handles the case where the formula is an enforce formula assigned true.
/// Result is not minimal.
pub fn find_strategy_moves_true_case<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    v0: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
) -> HashMap<StateIdx, PartialMove> {
    debug_assert!(v0.formula().is_enforce());
    debug_assert_eq!(assignments[v0], VertexAssignment::True);

    if v0.formula().is_invariant() {
        // EnforceInvariant has negation edge to DespiteEventually assigned undecided
        let edges = graph.succ(v0);
        let target = &edges[0].targets()[0];
        return find_strategy_moves_undecided_case(graph, target, assignments);
    }

    if v0.formula().is_next() {
        // Find first edge with all targets true
        for edge in graph.annotated_succ(v0) {
            let all_targets_true = edge
                .targets()
                .iter()
                .all(|(target, _)| assignments[target].is_true());
            if all_targets_true {
                let mut move_to_pick = HashMap::<StateIdx, PartialMove>::new();
                move_to_pick.insert(v0.state(), edge.annotation().unwrap().clone().unwrap());
                return move_to_pick;
            }
        }
        unreachable!(
            "True-assigned next formula should have at least one edge with all targets true"
        );
    }

    // Vertices that have been found to be part of the strategy
    let mut found = HashSet::<AtlVertex>::new();
    // Which move to pick for each state
    let mut move_to_pick = HashMap::<StateIdx, PartialMove>::new();
    // Vertices that may be part of strategy
    let mut verts_with_coalitions = Vec::<AtlVertex>::with_capacity(32);

    // Iterate assignments to find coalitions assigned true.
    // If their first edge is true, their strategy is trivial,
    // Otherwise we register them for the loop below.
    for (vert, ass) in assignments.iter() {
        if ass.is_true() && vert.formula().is_enforce() {
            let edges = graph.succ(vert);
            let phi2 = &edges[0].targets()[0];
            if assignments[phi2].is_true() {
                if vert == v0 {
                    return HashMap::new();
                }
                found.insert(vert.clone());
            } else {
                verts_with_coalitions.push(vert.clone());
            }
        }
    }

    // Iterate remaining coalitions to build branch with strategy
    loop {
        'next: for vert in &verts_with_coalitions {
            if move_to_pick.get(&vert.state()).is_none() {
                let edges = graph.annotated_succ(vert);
                for edge in &edges[1..] {
                    let all_targets_has_strat = edge.targets().iter().all(|(target, anno)| {
                        assignments[target].is_true() && (anno.is_none() || found.contains(target))
                    });

                    if !all_targets_has_strat {
                        continue;
                    }

                    // We found the partial move that will guarantee the property
                    move_to_pick.insert(vert.state(), edge.annotation().unwrap().clone().unwrap());
                    found.insert(vert.clone());

                    if vert == v0 {
                        // We are done
                        return move_to_pick;
                    }

                    continue 'next;
                }
            }
        }
    }
}

/// Compute the moves of the witness strategy.
/// This function handles the case where the formula is a despite formula assigned false.
/// Result is not minimal.
pub fn find_strategy_moves_false_case<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    v0: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
) -> HashMap<StateIdx, PartialMove> {
    debug_assert!(v0.formula().is_despite());
    debug_assert_eq!(assignments[v0], VertexAssignment::False);

    if v0.formula().is_invariant() {
        // DespiteInvariant has negation edge to EnforceEventually assigned true
        let edges = graph.succ(v0);
        let target = &edges[0].targets()[0];
        return find_strategy_moves_true_case(graph, target, assignments);
    }

    if v0.formula().is_next() {
        // Find first target assigned false
        let edges = graph.succ(v0);
        for target in edges[0].targets() {
            if assignments[target].is_false() {
                let mut move_to_pick = HashMap::<StateIdx, PartialMove>::new();
                move_to_pick.insert(v0.state(), target.partial_move().unwrap().clone());
                return move_to_pick;
            }
        }
    }

    debug_assert!(v0.formula().is_until()); // Eventually formula cannot be assigned false

    // Vertices that have been found to be part of the strategy
    let mut found = HashSet::<AtlVertex>::new();
    // Which move to pick for each state
    let mut move_to_pick = HashMap::<StateIdx, PartialMove>::new();
    // Vertices that may be part of strategy
    let mut verts_with_coalitions = Vec::<AtlVertex>::with_capacity(32);

    // Iterate assignments to find coalitions assigned false.
    // If the first target of their second edge is false, their strategy is trivial,
    // Otherwise we register them for the loop below.
    for (vert, ass) in assignments.iter() {
        if ass.is_false() && vert.is_full() && vert.formula().is_despite() {
            let edges = graph.succ(vert);
            let phi1 = &edges[1].targets()[0];
            if assignments[phi1].is_false() {
                if vert == v0 {
                    return HashMap::new();
                }
                found.insert(vert.clone());
            } else {
                verts_with_coalitions.push(vert.clone());
            }
        }
    }

    // Iterate remaining coalitions to build branch with strategy
    loop {
        'next: for vert in &verts_with_coalitions {
            if move_to_pick.get(&vert.state()).is_none() {
                let edges = graph.annotated_succ(vert);
                for (target, mov) in &edges[1].targets()[1..] {
                    if assignments[target].is_false() {
                        let edges_from_partial = graph.succ(target);
                        let all_dest_has_found_strat = edges_from_partial
                            .iter()
                            .all(|e| found.contains(&e.targets()[0]));
                        if all_dest_has_found_strat {
                            // We found the partial move that can break phi1
                            move_to_pick.insert(vert.state(), mov.clone().unwrap());
                            found.insert(vert.clone());

                            if vert == v0 {
                                // We are done
                                return move_to_pick;
                            }

                            continue 'next;
                        }
                    }
                }
            }
        }
    }
}

/// Compute the moves of the witness strategy.
/// This function handles the case where the formula is an DespiteUntil formula assigned undecided.
/// Result is not minimal.
pub fn find_strategy_moves_undecided_case<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    v0: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
) -> HashMap<StateIdx, PartialMove> {
    debug_assert!(v0.formula().is_despite());
    debug_assert_eq!(assignments[v0], VertexAssignment::Undecided);

    // Vertices that have been found to be part of the strategy
    let mut found = HashSet::<AtlVertex>::new();
    // Which move to pick for each state
    let mut move_to_pick = HashMap::<StateIdx, PartialMove>::new();
    // Vertices that will be part of strategy
    let mut verts_with_coalitions = Vec::<AtlVertex>::with_capacity(32);

    // Iterate assignments to find coalitions assigned false or undecided.
    // If the first target of their second edge is phi1 and false, their strategy is trivial,
    // Otherwise we register them for the loop below.
    for (vert, ass) in assignments.iter() {
        if !ass.is_true() && vert.is_full() && vert.formula().is_despite() {
            let edges = graph.succ(vert);
            let maybe_phi1 = &edges[1].targets()[0];
            if !maybe_phi1.formula().is_despite() && assignments[maybe_phi1].is_false() {
                if vert == v0 {
                    return HashMap::new();
                }
                found.insert(vert.clone());
            } else {
                verts_with_coalitions.push(vert.clone());
            }
        }
    }

    // Iterate remaining coalitions to find moves for the associated states.
    // No early termination since enemy may not play optimally, in which case we also
    // need to find the moves that break phi1 instead
    let mut remaining = verts_with_coalitions.len();
    while remaining > 0 {
        'next: for vert in &verts_with_coalitions {
            if move_to_pick.get(&vert.state()).is_none() {
                let edges = graph.annotated_succ(vert);
                if assignments[vert].is_false() {
                    // We are in a branch, where the coalition can force ph1 to be false eventually
                    for edge in &edges[1..] {
                        for (target, mov) in &edge.targets()[1..] {
                            if assignments[target].is_false() {
                                for e in graph.succ(target) {
                                    if e.targets().iter().all(|t| found.contains(t)) {
                                        // We found the partial move that can break phi1
                                        move_to_pick.insert(vert.state(), mov.clone().unwrap());
                                        found.insert(vert.clone());
                                        remaining -= 1;

                                        continue 'next;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // We are in a branch of undecided lassos, where the coalition can force phi2 to never be true
                    // Edge to phi1 may exist, and is true if is does
                    for (target, mov) in edges[1].targets() {
                        if !assignments[target].is_certain() {
                            // We do not need to ensure that we have found the strategy for the
                            // successors yet since the branch does not have to end in certainty.
                            // Hence, we found the partial move that for this state.
                            move_to_pick.insert(vert.state(), mov.clone().unwrap());
                            found.insert(vert.clone());
                            remaining -= 1;

                            continue 'next;
                        }
                    }
                }
            }
        }
    }

    move_to_pick
}
