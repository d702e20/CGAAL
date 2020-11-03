use crate::common::{Edges, HyperEdge, NegationEdge};
use crate::edg::ExtendedDependencyGraph;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::sync::Arc;

use crate::atl::common::{Player, State};
use crate::atl::formula::Phi;
use crate::atl::gamestructure::GameStructure;

#[derive(Clone, Debug)]
pub(crate) struct ATLDependencyGraph<G: GameStructure> {
    pub game_structure: G,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) enum ATLVertex {
    FULL {
        state: State,
        formula: Arc<Phi>,
    },
    PARTIAL {
        state: State,
        partial_move: PartialMove,
        formula: Arc<Phi>,
    },
}

impl ATLVertex {
    fn state(&self) -> State {
        match self {
            ATLVertex::FULL { state, .. } => *state,
            ATLVertex::PARTIAL { state, .. } => *state,
        }
    }

    fn formula(&self) -> Arc<Phi> {
        match self {
            ATLVertex::FULL { formula, .. } => formula.clone(),
            ATLVertex::PARTIAL { formula, .. } => formula.clone(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum PartialMoveChoice {
    /// Range from 0 to given number
    RANGE(usize),
    /// Chosen move for player
    SPECIFIC(usize),
}

pub type PartialMove = Vec<PartialMoveChoice>;

struct VarsIterator {
    moves: Vec<usize>,
    position: PartialMove,
    completed: bool,
}

impl VarsIterator {
    /// Iterates over all players choices for a set of players
    ///
    /// # Arguments
    ///
    /// * `moves` number of moves for each
    /// * `players` whoese move will be iterated over
    ///
    /// # Example
    ///
    /// ```
    /// use std::result::Result::{Some, None};
    ///
    /// let moves = vec![2, 3, 2]
    /// let mut players = HashSet::new();
    /// players.insert(0);
    /// players.insert(2);
    /// let iter = VarsIterator::new(moves, players);
    ///
    /// assert_eq!(iter.next(), Some(vec![PartialMoveChoice::SPECIFIC(0), PartialMoveChoice::RANGE(3), PartialMoveChoice::SPECIFIC(0)]))
    /// assert_eq!(iter.next(), Some(vec![PartialMoveChoice::SPECIFIC(1), PartialMoveChoice::RANGE(3), PartialMoveChoice::SPECIFIC(0)]))
    /// assert_eq!(iter.next(), Some(vec![PartialMoveChoice::SPECIFIC(0), PartialMoveChoice::RANGE(3), PartialMoveChoice::SPECIFIC(1)]))
    /// assert_eq!(iter.next(), Some(vec![PartialMoveChoice::SPECIFIC(1), PartialMoveChoice::RANGE(3), PartialMoveChoice::SPECIFIC(1)]))
    /// ```
    fn new(moves: Vec<usize>, players: HashSet<Player>) -> Self {
        let mut position = Vec::with_capacity(moves.len());
        for (i, mov) in moves.iter().enumerate() {
            position.push(if players.contains(&(i as usize)) {
                PartialMoveChoice::RANGE(*mov)
            } else {
                PartialMoveChoice::SPECIFIC(0)
            })
        }

        Self {
            moves,
            position,
            completed: false,
        }
    }
}

impl Iterator for VarsIterator {
    type Item = PartialMove;

    fn next(&mut self) -> Option<Self::Item> {
        if self.completed {
            return None;
        }

        let current = self.position.clone();

        let mut roll_over_pos = 0;
        loop {
            // If all digits have rolled over we reached the end
            if roll_over_pos >= self.moves.len() {
                self.completed = true;
                break;
            }

            match self.position[roll_over_pos] {
                PartialMoveChoice::RANGE(_) => {
                    roll_over_pos += 1;
                    continue;
                }
                PartialMoveChoice::SPECIFIC(value) => {
                    let new_value = value + 1;

                    if new_value >= self.moves[roll_over_pos] {
                        // Rolled over
                        self.position[roll_over_pos] = PartialMoveChoice::SPECIFIC(0);
                        roll_over_pos += 1;
                    } else {
                        self.position[roll_over_pos] = PartialMoveChoice::SPECIFIC(new_value);
                        break;
                    }
                }
            }
        }

        Some(current)
    }
}

struct DeltaIterator<'a, G: GameStructure> {
    game_structure: &'a G,
    state: State,
    moves: PartialMove,
    known: HashSet<State>,
    completed: bool,
    current_move: Vec<State>,
}

impl<'a, G: GameStructure> DeltaIterator<'a, G> {
    fn new(game_structure: &'a G, state: State, moves: PartialMove) -> Self {
        let known = HashSet::new();
        let mut current_move = Vec::with_capacity(moves.len());
        for mov in &moves {
            current_move.push(match mov {
                PartialMoveChoice::RANGE(_) => 0,
                PartialMoveChoice::SPECIFIC(i) => *i,
            });
        }

        Self {
            game_structure,
            state,
            moves,
            known,
            completed: false,
            current_move,
        }
    }

    /// Updates self.current_move to next position, or return false if the max position is reached.
    /// Returns false if the invocation produced the last move.
    fn next_move(&mut self) -> bool {
        let mut roll_over_pos = 0;
        loop {
            // If all digits have rolled over we reached the end
            if roll_over_pos >= self.moves.len() {
                self.completed = true;
                return false;
            }

            match self.moves[roll_over_pos] {
                PartialMoveChoice::SPECIFIC(_) => {
                    roll_over_pos += 1;
                }
                PartialMoveChoice::RANGE(cardinality) => {
                    let new_value = self.current_move[roll_over_pos] + 1;

                    if new_value >= cardinality {
                        // Rolled over
                        self.current_move[roll_over_pos] = 0;
                        roll_over_pos += 1;
                    } else {
                        self.current_move[roll_over_pos] = new_value;
                        break;
                    }
                }
            }
        }
        true
    }
}

impl<'a, G: GameStructure> Iterator for DeltaIterator<'a, G> {
    type Item = State;

    fn next(&mut self) -> Option<Self::Item> {
        if self.completed {
            return None;
        }

        loop {
            let target = self
                .game_structure
                .transitions(self.state, self.current_move.clone());

            println!("before: {:?}", self.current_move);
            let has_more_moves = self.next_move();
            println!(" after: {:?}", self.current_move);
            let is_known = self.known.contains(&target);

            if is_known && has_more_moves {
                continue;
            } else if is_known && !has_more_moves {
                assert!(self.completed); // Should be set by self.next_move()
                return None;
            } else {
                self.known.insert(target);
                return Some(target);
            }
        }
    }
}

impl<G: GameStructure> ATLDependencyGraph<G> {
    fn invert_players(&self, players: &Vec<Player>) -> HashSet<Player> {
        let max_players = self.game_structure.max_player() as usize;
        let mut inv_players =
            HashSet::with_capacity((self.game_structure.max_player() as usize) - players.len());
        // Iterate over all players and only add the ones not in players
        for player in 0usize..max_players {
            let player = player as usize;
            if players.contains(&player) {
                inv_players.insert(player);
            }
        }
        inv_players
    }
}

impl<G: GameStructure> ExtendedDependencyGraph<ATLVertex> for ATLDependencyGraph<G> {
    fn succ(&self, vert: &ATLVertex) -> HashSet<Edges<ATLVertex>, RandomState> {
        match vert {
            ATLVertex::FULL { state, formula } => match formula.as_ref() {
                Phi::PROPOSITION(prop) => {
                    let props = self.game_structure.labels(vert.state());
                    if props.contains(&prop) {
                        let mut edges: HashSet<Edges<ATLVertex>> = HashSet::new();
                        edges.insert(Edges::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets: vec![],
                        }));
                        edges
                    } else {
                        HashSet::new()
                    }
                }
                Phi::NOT(phi) => {
                    let mut edges: HashSet<Edges<ATLVertex>> = HashSet::new();

                    edges.insert(Edges::NEGATION(NegationEdge {
                        source: vert.clone(),
                        target: ATLVertex::FULL {
                            state: *state,
                            formula: phi.clone(),
                        },
                    }));

                    edges
                }
                Phi::OR(left, right) => {
                    let mut edges = HashSet::new();

                    let left_targets = vec![ATLVertex::FULL {
                        state: *state,
                        formula: left.clone(),
                    }];
                    edges.insert(Edges::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets: left_targets,
                    }));

                    let right_targets = vec![ATLVertex::FULL {
                        state: *state,
                        formula: right.clone(),
                    }];
                    edges.insert(Edges::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets: right_targets,
                    }));

                    edges
                }
                Phi::NEXT { players, formula } => {
                    let moves: Vec<usize> = self
                        .game_structure
                        .move_count(*state)
                        .iter()
                        .map(|&count| count as usize)
                        .collect();
                    VarsIterator::new(moves, players.iter().copied().collect())
                        .map(|pmove| {
                            let targets: Vec<ATLVertex> =
                                DeltaIterator::new(&self.game_structure, *state, pmove)
                                    .map(|state| ATLVertex::FULL {
                                        state,
                                        formula: formula.clone(),
                                    })
                                    .collect();
                            Edges::HYPER(HyperEdge {
                                source: vert.clone(),
                                targets,
                            })
                        })
                        .collect::<HashSet<Edges<ATLVertex>>>()
                }
                Phi::DESPITE_UNTIL {
                    players,
                    pre,
                    until,
                } => {
                    let inv_players = self.invert_players(players);
                    let mut edges = HashSet::new();

                    // hyper-edges with pre occurring
                    let pre = ATLVertex::FULL {
                        state: *state,
                        formula: pre.clone(),
                    };

                    let moves = self
                        .game_structure
                        .move_count(*state)
                        .iter()
                        .map(|&count| count as usize)
                        .collect();
                    let mut targets: Vec<ATLVertex> = VarsIterator::new(moves, inv_players)
                        .map(|pmove| ATLVertex::PARTIAL {
                            state: *state,
                            partial_move: pmove,
                            formula: vert.formula(),
                        })
                        .collect();
                    targets.push(pre);

                    edges.insert(Edges::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets,
                    }));

                    // Until without pre occurring
                    let targets = vec![ATLVertex::FULL {
                        state: *state,
                        formula: until.clone(),
                    }];
                    edges.insert(Edges::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets,
                    }));

                    edges
                }
                Phi::ENFORCE_UNTIL {
                    players,
                    pre,
                    until,
                } => {
                    // hyper-edges with pre occurring
                    let pre = ATLVertex::FULL {
                        state: *state,
                        formula: pre.clone(),
                    };

                    let moves = self
                        .game_structure
                        .move_count(*state)
                        .iter()
                        .map(|&count| count as usize)
                        .collect();
                    let mut edges: HashSet<Edges<ATLVertex>> = VarsIterator::new(
                        moves,
                        players.iter().map(|player| *player as usize).collect(),
                    )
                    .map(|pmove| {
                        let mut targets: Vec<ATLVertex> =
                            DeltaIterator::new(&self.game_structure, *state, pmove)
                                .map(|state| ATLVertex::FULL {
                                    state,
                                    formula: formula.clone(),
                                })
                                .collect();
                        targets.push(pre.clone());
                        Edges::HYPER(HyperEdge {
                            source: vert.clone(),
                            targets,
                        })
                    })
                    .collect();

                    // Until without pre occurring
                    let targets = vec![ATLVertex::FULL {
                        state: *state,
                        formula: until.clone(),
                    }];
                    edges.insert(Edges::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets,
                    }));

                    edges
                }
            },
            ATLVertex::PARTIAL {
                state,
                partial_move,
                formula,
            } => DeltaIterator::new(&self.game_structure, *state, partial_move.clone())
                .map(|state| {
                    let targets = vec![ATLVertex::FULL {
                        state,
                        formula: formula.clone(),
                    }];
                    Edges::HYPER(HyperEdge {
                        source: vert.clone(),
                        targets,
                    })
                })
                .collect::<HashSet<Edges<ATLVertex>>>(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::atl::common::DynVec;
    use crate::atl::dependencygraph::{DeltaIterator, PartialMoveChoice, VarsIterator};
    use crate::atl::gamestructure::EagerGameStructure;
    use std::collections::HashSet;
    use std::sync::Arc;

    #[test]
    fn vars_iterator() {
        let mut players = HashSet::new();
        players.insert(2);
        let mut iter = VarsIterator::new(vec![2, 3, 2], players);

        let value = iter.next().unwrap();
        println!("{:?}", value);
        assert_eq!(value[0], PartialMoveChoice::SPECIFIC(0));
        assert_eq!(value[1], PartialMoveChoice::SPECIFIC(0));
        assert_eq!(value[2], PartialMoveChoice::RANGE(2));

        let value = iter.next().unwrap();
        assert_eq!(value[0], PartialMoveChoice::SPECIFIC(1));
        assert_eq!(value[1], PartialMoveChoice::SPECIFIC(0));
        assert_eq!(value[2], PartialMoveChoice::RANGE(2));

        let value = iter.next().unwrap();
        assert_eq!(value[0], PartialMoveChoice::SPECIFIC(0));
        assert_eq!(value[1], PartialMoveChoice::SPECIFIC(1));
        assert_eq!(value[2], PartialMoveChoice::RANGE(2));

        let value = iter.next().unwrap();
        assert_eq!(value[0], PartialMoveChoice::SPECIFIC(1));
        assert_eq!(value[1], PartialMoveChoice::SPECIFIC(1));
        assert_eq!(value[2], PartialMoveChoice::RANGE(2));

        let value = iter.next().unwrap();
        assert_eq!(value[0], PartialMoveChoice::SPECIFIC(0));
        assert_eq!(value[1], PartialMoveChoice::SPECIFIC(2));
        assert_eq!(value[2], PartialMoveChoice::RANGE(2));

        let value = iter.next().unwrap();
        assert_eq!(value[0], PartialMoveChoice::SPECIFIC(1));
        assert_eq!(value[1], PartialMoveChoice::SPECIFIC(2));
        assert_eq!(value[2], PartialMoveChoice::RANGE(2));

        let value = iter.next();
        assert_eq!(value, None);
    }

    #[test]
    fn delta_iterator() {
        // player 0
        let transitions = DynVec::NEST(vec![
            // player 1
            Arc::new(DynVec::NEST(vec![
                // Player 2
                Arc::new(DynVec::NEST(vec![
                    // player 3
                    Arc::new(DynVec::NEST(vec![
                        // Player 4
                        Arc::new(DynVec::NEST(vec![Arc::new(DynVec::BASE(1))])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![Arc::new(DynVec::BASE(3))])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![Arc::new(DynVec::BASE(5))])),
                    ])),
                ])),
                // Player 2
                Arc::new(DynVec::NEST(vec![
                    // player 3
                    Arc::new(DynVec::NEST(vec![
                        // Player 4
                        Arc::new(DynVec::NEST(vec![Arc::new(DynVec::BASE(2))])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![Arc::new(DynVec::BASE(4))])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![Arc::new(DynVec::BASE(1))])),
                    ])),
                ])),
            ])),
        ]);
        let game_structure = EagerGameStructure {
            player_count: 5,
            labeling: vec![],
            transitions: vec![transitions],
            moves: vec![],
        };
        let state = 0;
        let partial_move = vec![
            PartialMoveChoice::SPECIFIC(0), // player 0
            PartialMoveChoice::RANGE(2),    // player 1
            PartialMoveChoice::SPECIFIC(0), // player 2
            PartialMoveChoice::RANGE(3),    // player 3
            PartialMoveChoice::SPECIFIC(0), // player 4
        ];
        let mut iter = DeltaIterator::new(&game_structure, state, partial_move);

        let value = iter.next().unwrap();
        assert_eq!(value, 1);

        let value = iter.next().unwrap();
        assert_eq!(value, 2);

        let value = iter.next().unwrap();
        assert_eq!(value, 3);

        let value = iter.next().unwrap();
        assert_eq!(value, 4);

        let value = iter.next().unwrap();
        assert_eq!(value, 5);

        // repeats state 1 again, but that is suppressed due to deduplication of emitted states

        let value = iter.next();
        assert_eq!(value, None);
    }
}
