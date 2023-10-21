use crate::edg::atledg::format::PartialMoveWithFormatting;
use crate::game_structure::{ActionIdx, GameStructure, PlayerIdx, StateIdx};
use std::collections::HashSet;
use std::convert::From;
use std::fmt::{Display, Formatter};
use std::ops::Index;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum PartialMoveChoice {
    /// Range from 0 to given number
    Range(usize),
    /// Chosen move for player
    Specific(ActionIdx),
}

impl PartialMoveChoice {
    pub fn unwrap_range(&self) -> usize {
        let PartialMoveChoice::Range(r) = self else {
            panic!("PartialMoveChoice was not a range of moves")
        };
        *r
    }

    pub fn unwrap_specific(&self) -> ActionIdx {
        let PartialMoveChoice::Specific(r) = self else {
            panic!("PartialMoveChoice was not a specific move")
        };
        *r
    }
}

impl Display for PartialMoveChoice {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialMoveChoice::Range(max) => write!(f, "(0..{})", max - 1),
            PartialMoveChoice::Specific(choice) => write!(f, "{}", choice),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct PartialMove(pub(crate) Vec<PartialMoveChoice>);

impl From<&[ActionIdx]> for PartialMove {
    fn from(v: &[ActionIdx]) -> Self {
        PartialMove(v.iter().map(|m| PartialMoveChoice::Specific(*m)).collect())
    }
}

impl Index<PlayerIdx> for PartialMove {
    type Output = PartialMoveChoice;

    fn index(&self, index: PlayerIdx) -> &Self::Output {
        &self.0[index.0]
    }
}

impl PartialMove {
    /// Creates a triple of a partial move with its game structure and the relevant state,
    /// allowing us to print the move using the names of players and moves as defined by
    /// the game structure.
    pub fn in_context_of<'a, G: GameStructure>(
        &'a self,
        game: &'a G,
        state: &'a StateIdx,
    ) -> PartialMoveWithFormatting<'a, G> {
        PartialMoveWithFormatting {
            pmove: self,
            game,
            state,
        }
    }
}

/// An iterator that produces all move vectors in a partial move.
/// Example: The partial move {1, 2},{1},{1, 2} results in 111, 112, 211, and 212.
pub(crate) struct PartialMoveIterator<'a> {
    partial_move: &'a PartialMove,
    initialized: bool,
    current: Vec<ActionIdx>,
}

impl<'a> PartialMoveIterator<'a> {
    /// Create a new PartialMoveIterator
    pub(crate) fn new(partial_move: &'a PartialMove) -> PartialMoveIterator {
        PartialMoveIterator {
            partial_move,
            initialized: false,
            current: vec![],
        }
    }

    /// Initializes the partial move iterator. This should be called exactly once
    /// before any call to make_next. This function creates the first move vector in a partial
    /// move. All partial move always contain at least one move vector.
    fn make_first(&mut self) {
        self.initialized = true;
        // Create the first move vector from matching on the partial move
        self.current = self
            .partial_move
            .0
            .iter()
            .map(|case| match case {
                PartialMoveChoice::Range(_) => ActionIdx(0),
                PartialMoveChoice::Specific(n) => *n,
            })
            .collect();
    }

    /// Updates self.current to the next move vector. This function returns false if a new
    /// move vector could not be created (due to exceeding the ranges in the partial move).
    fn make_next(&mut self, player: usize) -> bool {
        if player >= self.partial_move.0.len() {
            false

            // Call this function recursively, where we check the next player
        } else if !self.make_next(player + 1) {
            // The next player's move has rolled over or doesn't exist.
            // Then it is our turn to roll -- only RANGE can roll, SPECIFIC should not change
            match self.partial_move.0[player] {
                PartialMoveChoice::Specific(_) => false,
                PartialMoveChoice::Range(n) => {
                    let current = &mut self.current;
                    // Increase move index and return true if it's valid
                    current[player].0 += 1;
                    if current[player].0 < n {
                        true
                    } else {
                        // We have rolled over (self.next[player] >= n).
                        // Reset this player's move index and return false to indicate it
                        // was not possible to create a valid next move at this depth
                        current[player].0 = 0;
                        false
                    }
                }
            }
        } else {
            true
        }
    }
}

/// Allows the PartialMoveIterator to be iterated over.
impl<'a> Iterator for PartialMoveIterator<'a> {
    type Item = Vec<ActionIdx>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.initialized {
            self.make_first();
            Some(self.current.clone())
        } else if self.make_next(0) {
            Some(self.current.clone())
        } else {
            None
        }
    }
}

pub struct PmovesIterator {
    moves: Vec<usize>,
    position: PartialMove,
    completed: bool,
}

impl PmovesIterator {
    /// Iterates over all partial moves variants that results from a number of players
    /// making a combination of specific choices.
    ///
    /// # Arguments
    ///
    /// * `moves` number of moves for each player.
    /// * `players` set of players who has to make a specific move.
    ///
    pub fn new(moves: Vec<usize>, players: HashSet<PlayerIdx>) -> Self {
        let mut position = Vec::with_capacity(moves.len());
        for (i, mov) in moves.iter().enumerate() {
            position.push(if players.contains(&PlayerIdx(i)) {
                PartialMoveChoice::Specific(ActionIdx(0))
            } else {
                PartialMoveChoice::Range(*mov)
            })
        }

        Self {
            moves,
            position: PartialMove(position),
            completed: false,
        }
    }
}

impl Iterator for PmovesIterator {
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

            match self.position.0[roll_over_pos] {
                PartialMoveChoice::Range(_) => {
                    roll_over_pos += 1;
                    continue;
                }
                PartialMoveChoice::Specific(value) => {
                    let new_value = value.0 + 1;

                    if new_value >= self.moves[roll_over_pos] {
                        // Rolled over
                        self.position.0[roll_over_pos] = PartialMoveChoice::Specific(ActionIdx(0));
                        roll_over_pos += 1;
                    } else {
                        self.position.0[roll_over_pos] =
                            PartialMoveChoice::Specific(ActionIdx(new_value));
                        break;
                    }
                }
            }
        }

        Some(current)
    }
}

/// An iterator that produces all resulting states from taking a partial move at a state.
/// The iterator will make sure the same state is not produced multiple times, even if
/// it can be reached with different partial moves.
pub(crate) struct DeltaIterator<'a, G: GameStructure> {
    game_structure: &'a G,
    state: StateIdx,
    moves: PartialMoveIterator<'a>,
    /// Contains the states, that have already been produced once, so we can avoid producing
    /// them again
    known: HashSet<StateIdx>,
}

impl<'a, G: GameStructure> DeltaIterator<'a, G> {
    /// Create a new DeltaIterator
    pub(crate) fn new(game_structure: &'a G, state: StateIdx, moves: &'a PartialMove) -> Self {
        let known = HashSet::new();
        let moves = PartialMoveIterator::new(moves);

        Self {
            game_structure,
            state,
            moves,
            known,
        }
    }
}

impl<'a, G: GameStructure> Iterator for DeltaIterator<'a, G> {
    type Item = (StateIdx, PartialMove);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Get the next move vector from the partial move
            let mov = self.moves.next();
            if let Some(mov) = mov {
                let res = self.game_structure.get_successor(self.state, &mov);
                // Have we already produced this resulting state before?
                if self.known.contains(&res) {
                    continue;
                } else {
                    self.known.insert(res);
                    return Some((res, mov.as_slice().into()));
                }
            } else {
                return None;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use std::sync::Arc;

    use crate::edg::atledg::pmoves::{
        DeltaIterator, PartialMove, PartialMoveChoice, PartialMoveIterator, PmovesIterator,
    };
    use crate::game_structure::{ActionIdx, DynVec, EagerGameStructure, PlayerIdx, StateIdx};

    #[test]
    fn partial_move_iterator_01() {
        let partial_move = PartialMove(vec![
            PartialMoveChoice::Range(2),
            PartialMoveChoice::Specific(ActionIdx(1)),
            PartialMoveChoice::Range(2),
        ]);

        let mut iter = PartialMoveIterator::new(&partial_move);
        assert_eq!(
            iter.next(),
            Some(vec![ActionIdx(0), ActionIdx(1), ActionIdx(0)])
        );
        assert_eq!(
            iter.next(),
            Some(vec![ActionIdx(0), ActionIdx(1), ActionIdx(1)])
        );
        assert_eq!(
            iter.next(),
            Some(vec![ActionIdx(1), ActionIdx(1), ActionIdx(0)])
        );
        assert_eq!(
            iter.next(),
            Some(vec![ActionIdx(1), ActionIdx(1), ActionIdx(1)])
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn vars_iterator_01() {
        let moves = vec![2, 3, 2];
        let mut players = HashSet::new();
        players.insert(PlayerIdx(0));
        players.insert(PlayerIdx(2));
        let mut iter = PmovesIterator::new(moves, players);

        assert_eq!(
            &iter.next(),
            &Some(PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Range(3),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ]))
        );
        assert_eq!(
            &iter.next(),
            &Some(PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(1)),
                PartialMoveChoice::Range(3),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ]))
        );
        assert_eq!(
            &iter.next(),
            &Some(PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Range(3),
                PartialMoveChoice::Specific(ActionIdx(1)),
            ]))
        );
        assert_eq!(
            &iter.next(),
            &Some(PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(1)),
                PartialMoveChoice::Range(3),
                PartialMoveChoice::Specific(ActionIdx(1)),
            ]))
        );
    }

    #[test]
    fn vars_iterator_02() {
        let mut players = HashSet::new();
        players.insert(PlayerIdx(2));
        let mut iter = PmovesIterator::new(vec![2, 3, 3], players);

        let value = iter.next().unwrap();
        assert_eq!(value[PlayerIdx(0)], PartialMoveChoice::Range(2));
        assert_eq!(value[PlayerIdx(1)], PartialMoveChoice::Range(3));
        assert_eq!(
            value[PlayerIdx(2)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );

        let value = iter.next().unwrap();
        assert_eq!(value[PlayerIdx(0)], PartialMoveChoice::Range(2));
        assert_eq!(value[PlayerIdx(1)], PartialMoveChoice::Range(3));
        assert_eq!(
            value[PlayerIdx(2)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );

        let value = iter.next().unwrap();
        assert_eq!(value[PlayerIdx(0)], PartialMoveChoice::Range(2));
        assert_eq!(value[PlayerIdx(1)], PartialMoveChoice::Range(3));
        assert_eq!(
            value[PlayerIdx(2)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );

        let value = iter.next();
        assert_eq!(value, None);
    }

    #[test]
    fn vars_iterator_03() {
        // Both players choose. So we should end up with every move vector
        let mut players = HashSet::new();
        players.insert(PlayerIdx(0));
        players.insert(PlayerIdx(1));
        let mut iter = PmovesIterator::new(vec![3, 3], players);

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(0))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(1))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );

        let value = iter.next().unwrap();
        assert_eq!(
            value[PlayerIdx(0)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );
        assert_eq!(
            value[PlayerIdx(1)],
            PartialMoveChoice::Specific(ActionIdx(2))
        );

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn delta_iterator_01() {
        // player 0
        let transitions = DynVec::Nest(vec![
            // player 1
            Arc::new(DynVec::Nest(vec![
                // Player 2
                Arc::new(DynVec::Nest(vec![
                    // player 3
                    Arc::new(DynVec::Nest(vec![
                        // Player 4
                        Arc::new(DynVec::Nest(vec![Arc::new(DynVec::Base(StateIdx(1)))])),
                        // Player 4
                        Arc::new(DynVec::Nest(vec![Arc::new(DynVec::Base(StateIdx(2)))])),
                        // Player 4
                        Arc::new(DynVec::Nest(vec![Arc::new(DynVec::Base(StateIdx(3)))])),
                    ])),
                ])),
                // Player 2
                Arc::new(DynVec::Nest(vec![
                    // player 3
                    Arc::new(DynVec::Nest(vec![
                        // Player 4
                        Arc::new(DynVec::Nest(vec![Arc::new(DynVec::Base(StateIdx(4)))])),
                        // Player 4
                        Arc::new(DynVec::Nest(vec![Arc::new(DynVec::Base(StateIdx(5)))])),
                        // Player 4
                        Arc::new(DynVec::Nest(vec![Arc::new(DynVec::Base(StateIdx(1)))])),
                    ])),
                ])),
            ])),
        ]);
        let game = EagerGameStructure {
            initial_state: StateIdx(0),
            player_count: 5,
            labeling: vec![],
            transitions: vec![transitions],
            moves: vec![],
        };
        let partial_move = PartialMove(vec![
            PartialMoveChoice::Specific(ActionIdx(0)), // player 0
            PartialMoveChoice::Range(2),               // player 1
            PartialMoveChoice::Specific(ActionIdx(0)), // player 2
            PartialMoveChoice::Range(3),               // player 3
            PartialMoveChoice::Specific(ActionIdx(0)), // player 4
        ]);
        let mut iter = DeltaIterator::new(&game, game.initial_state, &partial_move);

        let (state, pmove) = iter.next().unwrap();
        assert_eq!(state, StateIdx(1));
        assert_eq!(
            pmove,
            PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ])
        );

        let (state, pmove) = iter.next().unwrap();
        assert_eq!(state, StateIdx(2));
        assert_eq!(
            pmove,
            PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(1)),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ])
        );

        let (state, pmove) = iter.next().unwrap();
        assert_eq!(state, StateIdx(3));
        assert_eq!(
            pmove,
            PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(2)),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ])
        );

        let (state, pmove) = iter.next().unwrap();
        assert_eq!(state, StateIdx(4));
        assert_eq!(
            pmove,
            PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(1)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ])
        );

        let (state, pmove) = iter.next().unwrap();
        assert_eq!(state, StateIdx(5));
        assert_eq!(
            pmove,
            PartialMove(vec![
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(1)),
                PartialMoveChoice::Specific(ActionIdx(0)),
                PartialMoveChoice::Specific(ActionIdx(1)),
                PartialMoveChoice::Specific(ActionIdx(0)),
            ])
        );

        // repeats state 1 again, but that is suppressed due to deduplication of emitted states

        assert_eq!(iter.next(), None);
    }
}
