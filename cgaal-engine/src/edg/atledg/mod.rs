use crate::atl::Phi;
use crate::edg::atledg::pmoves::{DeltaIterator, PmovesIterator};
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::{Edge, ExtendedDependencyGraph, HyperEdge, NegationEdge};
use crate::game_structure::{GameStructure, PlayerIdx};
use std::collections::HashSet;
use std::sync::Arc;

pub mod annotated;
pub mod format;
pub mod pmoves;
pub mod vertex;

#[derive(Clone, Debug)]
pub struct AtlDependencyGraph<G: GameStructure> {
    pub game_structure: G,
}

impl<G: GameStructure> AtlDependencyGraph<G> {
    #[allow(dead_code)]
    fn invert_players(&self, players: &[PlayerIdx]) -> HashSet<PlayerIdx> {
        let player_count = self.game_structure.player_count();
        let mut inv_players = HashSet::with_capacity(player_count - players.len());
        // Iterate over all players and only add the ones not in players
        for player in 0usize..player_count {
            if players.contains(&PlayerIdx(player)) {
                inv_players.insert(PlayerIdx(player));
            }
        }
        inv_players
    }
}

impl<G: GameStructure> ExtendedDependencyGraph<AtlVertex> for AtlDependencyGraph<G> {
    /// Produce the edges of the given vertex
    /// Where possible, the smallest edge will be the first in the produced vector,
    /// and similarly, the smallest target will be the first in the edges' vector of targets.
    /// This is mostly relevant for the Until formulae
    fn succ(&self, vert: &AtlVertex) -> Vec<Edge<AtlVertex>> {
        match vert {
            AtlVertex::Full { state, formula } => match formula.as_ref() {
                Phi::True => {
                    // Hyper edge with no targets
                    vec![Edge::Hyper(HyperEdge {
                        source: vert.clone(),
                        targets: vec![],
                    })]
                }
                Phi::False => {
                    // No edges
                    vec![]
                }
                Phi::Proposition(prop) => {
                    let props = self.game_structure.labels(vert.state());
                    if props.contains(prop) {
                        vec![Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![],
                        })]
                    } else {
                        vec![]
                    }
                }
                Phi::Not(phi) => {
                    vec![Edge::Negation(NegationEdge {
                        source: vert.clone(),
                        target: AtlVertex::Full {
                            state: *state,
                            formula: phi.clone(),
                        },
                    })]
                }
                Phi::Or(left, right) => {
                    vec![
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![AtlVertex::Full {
                                state: *state,
                                formula: left.clone(),
                            }],
                        }),
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![AtlVertex::Full {
                                state: *state,
                                formula: right.clone(),
                            }],
                        }),
                    ]
                }
                Phi::And(left, right) => {
                    vec![Edge::Hyper(HyperEdge {
                        source: vert.clone(),
                        targets: vec![
                            AtlVertex::Full {
                                state: *state,
                                formula: left.clone(),
                            },
                            AtlVertex::Full {
                                state: *state,
                                formula: right.clone(),
                            },
                        ],
                    })]
                }
                Phi::DespiteNext { players, formula } => {
                    let moves = self.game_structure.move_count(*state);
                    let targets: Vec<AtlVertex> =
                        PmovesIterator::new(moves, players.iter().copied().collect())
                            .map(|pmove| AtlVertex::Partial {
                                state: *state,
                                partial_move: pmove,
                                formula: formula.clone(),
                            })
                            .collect();

                    vec![Edge::Hyper(HyperEdge {
                        source: vert.clone(),
                        targets,
                    })]
                }
                Phi::EnforceNext { players, formula } => {
                    let moves = self.game_structure.move_count(*state);
                    PmovesIterator::new(moves, players.iter().copied().collect())
                        .map(|pmove| {
                            let targets: Vec<AtlVertex> =
                                DeltaIterator::new(&self.game_structure, *state, &pmove)
                                    .map(|(state, _)| AtlVertex::Full {
                                        state,
                                        formula: formula.clone(),
                                    })
                                    .collect();
                            Edge::Hyper(HyperEdge {
                                source: vert.clone(),
                                targets,
                            })
                        })
                        .collect::<Vec<Edge<AtlVertex>>>()
                }
                Phi::DespiteUntil {
                    players,
                    pre,
                    until,
                } => {
                    // `pre`-target
                    // "Is `pre` formula satisfied now?"
                    let pre = AtlVertex::Full {
                        state: *state,
                        formula: pre.clone(),
                    };

                    // Together with the `pre` target is all the possible moves by other players,
                    // but it is important that `pre` is the first target
                    let moves = self.game_structure.move_count(*state);
                    let targets: Vec<AtlVertex> = std::iter::once(pre)
                        .chain(
                            PmovesIterator::new(moves, players.iter().cloned().collect()).map(
                                |pmove| AtlVertex::Partial {
                                    state: *state,
                                    partial_move: pmove,
                                    formula: vert.formula(),
                                },
                            ),
                        )
                        .collect();

                    vec![
                        // `until`-formula branch
                        // "Is the `until` formula satisfied now?"
                        // This must be the first edge
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![AtlVertex::Full {
                                state: *state,
                                formula: until.clone(),
                            }],
                        }),
                        // Other branches where pre is satisfied
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets,
                        }),
                    ]
                }
                Phi::EnforceUntil {
                    players,
                    pre,
                    until,
                } => {
                    let mut edges = vec![
                        // `until`-formula branch
                        // "Is the `until` formula satisfied now?"
                        // This must be the first edge
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![AtlVertex::Full {
                                state: *state,
                                formula: until.clone(),
                            }],
                        }),
                    ];

                    // `pre`-target
                    // "Is `pre` formula satisfied now?"
                    let pre = AtlVertex::Full {
                        state: *state,
                        formula: pre.clone(),
                    };

                    let moves = self.game_structure.move_count(*state);
                    edges.extend(
                        PmovesIterator::new(moves, players.iter().copied().collect()).map(
                            |pmove| {
                                // Together with the `pre` target is all the possible moves by other players,
                                // but it is important that `pre` is the first target
                                let delta =
                                    DeltaIterator::new(&self.game_structure, *state, &pmove).map(
                                        |(state, _)| AtlVertex::Full {
                                            state,
                                            formula: formula.clone(),
                                        },
                                    );
                                let targets: Vec<AtlVertex> =
                                    std::iter::once(pre.clone()).chain(delta).collect();
                                Edge::Hyper(HyperEdge {
                                    source: vert.clone(),
                                    targets,
                                })
                            },
                        ),
                    );

                    edges
                }
                Phi::DespiteEventually {
                    players,
                    formula: subformula,
                } => {
                    // Partial targets with same formula
                    // "Is the formula satisfied in the next state instead?"
                    let moves = self.game_structure.move_count(*state);
                    let targets: Vec<AtlVertex> =
                        PmovesIterator::new(moves, players.iter().cloned().collect())
                            .map(|pmove| AtlVertex::Partial {
                                state: *state,
                                partial_move: pmove,
                                formula: formula.clone(),
                            })
                            .collect();

                    vec![
                        // sub-formula target
                        // "Is the sub formula satisfied in current state?"
                        // This must be the first edge
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![AtlVertex::Full {
                                state: *state,
                                formula: subformula.clone(),
                            }],
                        }),
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets,
                        }),
                    ]
                }
                Phi::EnforceEventually {
                    players,
                    formula: subformula,
                } => {
                    let mut edges = vec![
                        // sub-formula target
                        // "Is the sub formula satisfied in current state?"
                        // This must be the first edge
                        Edge::Hyper(HyperEdge {
                            source: vert.clone(),
                            targets: vec![AtlVertex::Full {
                                state: *state,
                                formula: subformula.clone(),
                            }],
                        }),
                    ];

                    // Successor states with same formula
                    // "Is the formula satisfied in the next state instead?"
                    let moves = self.game_structure.move_count(*state);
                    edges.extend(
                        PmovesIterator::new(moves, players.iter().copied().collect()).map(
                            |pmove| {
                                let targets: Vec<AtlVertex> =
                                    DeltaIterator::new(&self.game_structure, *state, &pmove)
                                        .map(|(state, _)| AtlVertex::Full {
                                            state,
                                            formula: formula.clone(),
                                        })
                                        .collect();
                                Edge::Hyper(HyperEdge {
                                    source: vert.clone(),
                                    targets,
                                })
                            },
                        ),
                    );

                    edges
                }
                Phi::DespiteInvariant {
                    players,
                    formula: subformula,
                } => {
                    vec![Edge::Negation(NegationEdge {
                        source: vert.clone(),
                        target: AtlVertex::Full {
                            state: *state,
                            // Modified formula, switching to minimum-fixed point domain
                            formula: Arc::new(Phi::EnforceEventually {
                                players: players.clone(),
                                formula: Arc::new(Phi::Not(subformula.clone())),
                            }),
                        },
                    })]
                }
                Phi::EnforceInvariant {
                    players,
                    formula: subformula,
                } => {
                    vec![Edge::Negation(NegationEdge {
                        source: vert.clone(),
                        target: AtlVertex::Full {
                            state: *state,
                            // Modified formula, switching to minimum-fixed point
                            formula: Arc::new(Phi::DespiteEventually {
                                players: players.clone(),
                                formula: Arc::new(Phi::Not(subformula.clone())),
                            }),
                        },
                    })]
                }
            },
            AtlVertex::Partial {
                state,
                partial_move,
                formula,
            } => DeltaIterator::new(&self.game_structure, *state, partial_move)
                .map(|(state, _)| {
                    let targets = vec![AtlVertex::Full {
                        state,
                        formula: formula.clone(),
                    }];
                    Edge::Hyper(HyperEdge {
                        source: vert.clone(),
                        targets,
                    })
                })
                .collect::<Vec<Edge<AtlVertex>>>(),
        }
    }
}
