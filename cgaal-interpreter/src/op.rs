use cgaal_engine::game_structure::{Action, GameStructure, State};

pub enum Op {
    Move { moves: Vec<Action> },
    Players,
    Skip,
}
impl Op {
    pub fn run<G: GameStructure>(cgs: &G, state: State, op: Op) -> Result<State, String> {
        match op {
            // Do nothing
            Op::Skip => Ok(state),
            // Do a transition given a move vector
            Op::Move { moves } => Ok(cgs.transitions(state, moves)),
            // Print all players with human readable names
            Op::Players => {
                println!("[");
                for i in 0..cgs.max_player() {
                    println!(" {}: {}", i, cgs.player_name(i));
                }
                println!("]");
                Ok(state)
            }
        }
    }
}
