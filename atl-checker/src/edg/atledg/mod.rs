use crate::atl::Phi;
use crate::edg::atledg::pmoves::{DeltaIterator, PmovesIterator};
use crate::edg::atledg::vertex::ATLVertex;
use crate::edg::{Edge, ExtendedDependencyGraph, HyperEdge, NegationEdge};
use crate::game_structure::{GameStructure, Player};
use std::collections::HashSet;
use std::sync::Arc;

pub mod annotated;
mod pmoves;
pub mod vertex;

#[derive(Clone, Debug)]
pub struct ATLDependencyGraph<G: GameStructure> {
    pub game_structure: G,
}

impl<G: GameStructure> ATLDependencyGraph<G> {
    #[allow(dead_code)]
    fn invert_players(&self, players: &[Player]) -> HashSet<Player> {
        let max_players = self.game_structure.max_player();
        let mut inv_players =
            HashSet::with_capacity((self.game_structure.max_player()) - players.len());
        // Iterate over all players and only add the ones not in players
        for player in 0usize..max_players {
            if players.contains(&player) {
                inv_players.insert(player);
            }
        }
        inv_players
    }
}

impl<G: GameStructure> ExtendedDependencyGraph<ATLVertex> for ATLDependencyGraph<G> {
    /// Produce the edges of the given vertex
    /// Where possible, the smallest edge will be the first in the produced vector,
    /// and similarly, the smallest target will be the first in the edges' vector of targets.
    /// This is mostly relevant for the Until formulae
    fn succ(&self, vert: &ATLVertex) -> Vec<Edge<ATLVertex>> {
        match vert {
            ATLVertex::FULL { state, formula } => match formula.as_ref() {
                Phi::True => {
                    // Hyper edge with no targets
                    vec![Edge::HYPER(HyperEdge {
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
                        vec![Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![],
                        })]
                    } else {
                        vec![]
                    }
                }
                Phi::Not(phi) => {
                    vec![Edge::NEGATION(NegationEdge {
                        source: vert.clone(),
                        target: ATLVertex::FULL {
                            state: *state,
                            formula: phi.clone(),
                        },
                    })]
                }
                Phi::Or(left, right) => {
                    vec![
                        Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![ATLVertex::FULL {
                                state: *state,
                                formula: left.clone(),
                            }],
                        }),
                        Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![ATLVertex::FULL {
                                state: *state,
                                formula: right.clone(),
                            }],
                        }),
                    ]
                }
                Phi::And(left, right) => {
                    vec![Edge::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets: vec![
                            ATLVertex::FULL {
                                state: *state,
                                formula: left.clone(),
                            },
                            ATLVertex::FULL {
                                state: *state,
                                formula: right.clone(),
                            },
                        ],
                    })]
                }
                Phi::DespiteNext { players, formula } => {
                    let moves = self.game_structure.move_count(*state);
                    let targets: Vec<ATLVertex> =
                        PmovesIterator::new(moves, players.iter().copied().collect())
                            .map(|pmove| ATLVertex::PARTIAL {
                                state: *state,
                                partial_move: pmove,
                                formula: formula.clone(),
                            })
                            .collect();

                    vec![Edge::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets,
                    })]
                }
                Phi::EnforceNext { players, formula } => {
                    let moves = self.game_structure.move_count(*state);
                    PmovesIterator::new(moves, players.iter().copied().collect())
                        .map(|pmove| {
                            let targets: Vec<ATLVertex> =
                                DeltaIterator::new(&self.game_structure, *state, &pmove)
                                    .map(|(state, _)| ATLVertex::FULL {
                                        state,
                                        formula: formula.clone(),
                                    })
                                    .collect();
                            Edge::HYPER(HyperEdge {
                                source: vert.clone(),
                                targets,
                            })
                        })
                        .collect::<Vec<Edge<ATLVertex>>>()
                }
                Phi::DespiteUntil {
                    players,
                    pre,
                    until,
                } => {
                    // `pre`-target
                    // "Is `pre` formula satisfied now?"
                    let pre = ATLVertex::FULL {
                        state: *state,
                        formula: pre.clone(),
                    };

                    // Together with the `pre` target is all the possible moves by other players,
                    // but it is important that `pre` is the first target
                    let moves = self.game_structure.move_count(*state);
                    let targets: Vec<ATLVertex> = std::iter::once(pre)
                        .chain(
                            PmovesIterator::new(moves, players.iter().cloned().collect()).map(
                                |pmove| ATLVertex::PARTIAL {
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
                        Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![ATLVertex::FULL {
                                state: *state,
                                formula: until.clone(),
                            }],
                        }),
                        // Other branches where pre is satisfied
                        Edge::HYPER(HyperEdge {
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
                        Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![ATLVertex::FULL {
                                state: *state,
                                formula: until.clone(),
                            }],
                        }),
                    ];

                    // `pre`-target
                    // "Is `pre` formula satisfied now?"
                    let pre = ATLVertex::FULL {
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
                                        |(state, _)| ATLVertex::FULL {
                                            state,
                                            formula: formula.clone(),
                                        },
                                    );
                                let targets: Vec<ATLVertex> =
                                    std::iter::once(pre.clone()).chain(delta).collect();
                                Edge::HYPER(HyperEdge {
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
                    let targets: Vec<ATLVertex> =
                        PmovesIterator::new(moves, players.iter().cloned().collect())
                            .map(|pmove| ATLVertex::PARTIAL {
                                state: *state,
                                partial_move: pmove,
                                formula: formula.clone(),
                            })
                            .collect();

                    vec![
                        // sub-formula target
                        // "Is the sub formula satisfied in current state?"
                        // This must be the first edge
                        Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![ATLVertex::FULL {
                                state: *state,
                                formula: subformula.clone(),
                            }],
                        }),
                        Edge::HYPER(HyperEdge {
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
                        Edge::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![ATLVertex::FULL {
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
                                let targets: Vec<ATLVertex> =
                                    DeltaIterator::new(&self.game_structure, *state, &pmove)
                                        .map(|(state, _)| ATLVertex::FULL {
                                            state,
                                            formula: formula.clone(),
                                        })
                                        .collect();
                                Edge::HYPER(HyperEdge {
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
                    vec![Edge::NEGATION(NegationEdge {
                        source: vert.clone(),
                        target: ATLVertex::FULL {
                            state: *state,
                            // Modified formula, switching to minimum-fixed point domain
                            formula: Arc::new(Phi::EnforceUntil {
                                players: players.clone(),
                                pre: Arc::new(Phi::True),
                                until: Arc::new(Phi::Not(subformula.clone())),
                            }),
                        },
                    })]
                }
                Phi::EnforceInvariant {
                    players,
                    formula: subformula,
                } => {
                    vec![Edge::NEGATION(NegationEdge {
                        source: vert.clone(),
                        target: ATLVertex::FULL {
                            state: *state,
                            // Modified formula, switching to minimum-fixed point
                            formula: Arc::new(Phi::DespiteUntil {
                                players: players.clone(),
                                pre: Arc::new(Phi::True),
                                until: Arc::new(Phi::Not(subformula.clone())),
                            }),
                        },
                    })]
                }
            },
            ATLVertex::PARTIAL {
                state,
                partial_move,
                formula,
            } => DeltaIterator::new(&self.game_structure, *state, partial_move)
                .map(|(state, _)| {
                    let targets = vec![ATLVertex::FULL {
                        state,
                        formula: formula.clone(),
                    }];
                    Edge::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets,
                    })
                })
                .collect::<Vec<Edge<ATLVertex>>>(),
        }
    }
}
