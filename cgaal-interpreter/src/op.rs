use cgaal_engine::game_structure::{Action, GameStructure, State};

pub enum Op {
    Move { moves: Vec<Action> },
}

pub fn run<G: GameStructure>(cgs: &G, state: State, op: Op) -> Result<State, String> {
    match op {
        Op::Move { moves } => Ok(cgs.transitions(state, moves)),
    }
}
