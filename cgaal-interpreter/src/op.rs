use crate::parser::TypedValue;
use cgaal_engine::game_structure::{Action};

pub enum Op {
    Skip,
    Move { moves: Vec<Action> },
    DisplayPlayers,
    DisplayPlayer { player: TypedValue },
}
