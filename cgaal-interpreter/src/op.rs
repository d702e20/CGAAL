use crate::parser::TypedValue;
use cgaal_engine::game_structure::Action;

pub enum Op {
    Skip,
    Move { moves: Vec<Action> },
    Display { value: TypedValue },
    DisplayAll { value: TypedValue },
}
