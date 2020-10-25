use crate::atl::PartialMoveChoice::SPECIFIC;
use crate::common::{Edges, HyperEdge, NegationEdge};
use crate::edg::ExtendedDependencyGraph;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::sync::Arc;

type Proposition = usize;
type Player = usize;
type State = usize;
/// A given choice for all players
type MoveVector = Vec<usize>;

#[derive(Hash, Eq, PartialEq, Clone)]
enum Phi {
    TRUE,
    PROPOSITION(Proposition),
    NOT(Arc<Phi>),
    AND(Arc<Phi>, Arc<Phi>),
    NEXT(Player, Arc<Phi>),
    UNTIL {
        player: Player,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
}

#[derive(Clone)]
struct EagerGameStructure {
    /// K, number of players
    player_count: u32,
    /// Maps states to Vec of atomic proposition, aka the labeling function
    labeling: Vec<HashSet<Proposition>>,
    /// Maps states, then players recursively
    transitions: Vec<DynVec>,
    /// available moves for a player in a given state
    moves: Vec<Vec<u32>>,
}

fn transition_lookup(choices: &[usize], transitions: &DynVec) -> State {
    match transitions {
        DynVec::NEST(v) => {
            if choices.len() == 0 {
                panic!("Fewer choices given than number of players in transitions");
            }

            println!("choice: {:?}", choices[0]);
            let choice = choices[0];
            let h: &DynVec = v.get(choice).expect("Out of bounds choice");

            transition_lookup(&choices[1..choices.len()], h)
        }
        DynVec::BASE(state) => *state,
    }
}

impl<'a> GameStructure<'a> for EagerGameStructure {
    fn max_player(&self) -> u32 {
        self.player_count
    }

    fn labels(&self, state: usize) -> &'a HashSet<Proposition, RandomState> {
        todo!()
        //self.labeling.get(state).expect(format!("").as_str())
    }

    fn transitions(&self, state: State, choices: Vec<usize>) -> State {
        transition_lookup(
            choices.as_slice(),
            &self
                .transitions
                .get(state)
                .expect(format!("Undefined state {}, no transitions", state).as_str()),
        )
    }

    fn available_moves(&self, state: usize, player: usize) -> u32 {
        *self
            .moves
            .get(state)
            .expect(format!("Requested move for non-existent state {}", state).as_str())
            .get(player)
            .expect(
                format!(
                    "Request move for non-existent player {} from state {}",
                    player, state
                )
                .as_str(),
            )
    }
}

trait GameStructure<'a> {
    fn max_player(&self) -> u32;

    fn labels(&self, state: usize) -> &'a HashSet<Proposition>;

    fn transitions(&self, state: State, choices: Vec<usize>) -> State;

    fn available_moves(&self, state: State, player: Player) -> u32;
}

struct ATLDependencyGraph<'a, G: GameStructure<'a>> {
    formula: Phi,
    game_structure: G,
    phantom: PhantomData<&'a G>,
}

#[derive(Clone, Hash, Eq, PartialEq)]
struct ATLVertex {
    state: State,
    formula: Arc<Phi>,
}

#[derive(Clone)]
enum DynVec {
    NEST(Vec<Arc<DynVec>>),
    BASE(State),
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum PartialMoveChoice {
    /// Range from 0 to given number
    RANGE(usize),
    /// Chosen move for player
    SPECIFIC(usize),
}

type PartialMove = Vec<PartialMoveChoice>;

type SpecificMove = Vec<usize>;

struct VarsIterator {
    moves: Vec<usize>,
    players: HashSet<Player>,
    position: PartialMove,
    completed: bool,
}

impl VarsIterator {
    fn new(moves: Vec<usize>, players: HashSet<Player>) -> Self {
        let mut position = Vec::with_capacity(moves.len());
        for i in 0..moves.len() {
            position.push(if players.contains(&(i as usize)) {
                PartialMoveChoice::RANGE(moves[i])
            } else {
                PartialMoveChoice::SPECIFIC(0)
            })
        }

        Self {
            moves,
            players,
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
                        self.position[roll_over_pos] = SPECIFIC(0);
                        roll_over_pos += 1;
                    } else {
                        self.position[roll_over_pos] = SPECIFIC(new_value);
                        break;
                    }
                }
            }
        }

        Some(current)
    }
}

struct DeltaIterator {
    transitions: DynVec,
    moves: PartialMove,
    known: HashSet<State>,
    completed: bool,
    current_move: Vec<State>,
}

impl DeltaIterator {
    fn new(transitions: DynVec, moves: PartialMove) -> Self {
        let known = HashSet::new();
        let mut current_move = Vec::with_capacity(moves.len());
        for i in 0..moves.len() {
            current_move.push(0);
        }

        Self {
            transitions,
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
                PartialMoveChoice::RANGE(max) => {
                    let new_value = self.current_move[roll_over_pos] + 1;

                    if new_value >= max {
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
        return true;
    }
}

impl Iterator for DeltaIterator {
    type Item = State;

    fn next(&mut self) -> Option<Self::Item> {
        if self.completed {
            return None;
        }

        loop {
            let target = transition_lookup(self.current_move.as_slice(), &self.transitions);

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

impl<'a, G: GameStructure<'a>> ATLDependencyGraph<'a, G> {
    fn vars(
        &self,
        // Number of moves for each player
        moves: Vec<usize>,
        players: HashSet<Player>,
    ) -> Box<dyn Iterator<Item = PartialMove>> {
        return Box::new(VarsIterator::new(moves, players));
    }

    fn delta(&self, transitions: DynVec, moves: PartialMove) -> Box<dyn Iterator<Item = State>> {
        return Box::new(DeltaIterator::new(transitions, moves));
    }
}

impl<'a, G: GameStructure<'a>> ExtendedDependencyGraph<ATLVertex> for ATLDependencyGraph<'a, G> {
    fn succ(&self, vert: &ATLVertex) -> HashSet<Edges<ATLVertex>, RandomState> {
        match vert.formula.as_ref() {
            Phi::TRUE => {
                let mut edges: HashSet<Edges<ATLVertex>> = HashSet::new();
                edges.insert(Edges::HYPER(HyperEdge {
                    source: vert.clone(),
                    targets: vec![],
                }));
                edges
            }
            Phi::PROPOSITION(prop) => {
                let props = self.game_structure.labels(vert.state);
                if props.contains(prop) {
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
                    target: ATLVertex {
                        state: vert.state,
                        formula: phi.clone(),
                    },
                }));
                edges
            }
            Phi::AND(left, right) => {
                let mut edges = HashSet::new();

                let left_targets = vec![ATLVertex {
                    state: vert.state,
                    formula: left.clone(),
                }];
                edges.insert(Edges::HYPER(HyperEdge {
                    source: vert.clone(),
                    targets: left_targets,
                }));

                let right_targets = vec![ATLVertex {
                    state: vert.state,
                    formula: right.clone(),
                }];
                edges.insert(Edges::HYPER(HyperEdge {
                    source: vert.clone(),
                    targets: right_targets,
                }));

                edges
            }
            Phi::NEXT(player, formula) => {
                let edges = HashSet::new();

                todo!("figure 2, circle");

                edges
            }
            Phi::UNTIL { player, pre, until } => {
                let mut edges: HashSet<Edges<ATLVertex>> = HashSet::new();

                // Until without pre occurring
                let targets = vec![ATLVertex {
                    state: vert.state,
                    formula: until.clone(),
                }];
                edges.insert(Edges::HYPER(HyperEdge {
                    source: vert.clone(),
                    targets,
                }));

                // hyper-edges with pre occurring
                //todo!();

                edges
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::atl::{DeltaIterator, DynVec, PartialMoveChoice, VarsIterator};
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
                        Arc::new(DynVec::NEST(vec![
                            Arc::new(DynVec::BASE(1)),
                        ])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![
                            Arc::new(DynVec::BASE(3)),
                        ])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![
                            Arc::new(DynVec::BASE(5)),
                        ])),
                    ])),
                ])),
                // Player 2
                Arc::new(DynVec::NEST(vec![
                    // player 3
                    Arc::new(DynVec::NEST(vec![
                        // Player 4
                        Arc::new(DynVec::NEST(vec![
                            Arc::new(DynVec::BASE(2)),
                        ])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![
                            Arc::new(DynVec::BASE(4)),
                        ])),
                        // Player 4
                        Arc::new(DynVec::NEST(vec![
                            Arc::new(DynVec::BASE(1)),
                        ])),
                    ])),
                ])),
            ])),
        ]);
        let state = 0;
        let partial_move = vec![
            PartialMoveChoice::SPECIFIC(0), // player 0
            PartialMoveChoice::RANGE(2),    // player 1
            PartialMoveChoice::SPECIFIC(0), // player 2
            PartialMoveChoice::RANGE(3),    // player 3
            PartialMoveChoice::SPECIFIC(0), // player 4
        ];
        let mut iter = DeltaIterator::new(transitions, partial_move);

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
