use cgaal_engine::game_structure::{Action, GameStructure, State};
use std::fs::File;
use std::io::Read;

pub enum Op {
    Move { moves: Vec<Action> },
}

pub fn run<G: GameStructure>(mut cgs: &G, state: State, op: Op) -> Result<State, String> {
    return match op {
        Op::Move { moves } => Ok(cgs.transitions(state, moves)),
    };
}
