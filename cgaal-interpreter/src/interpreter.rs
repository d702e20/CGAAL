use crate::op::Op;
use crate::parser::{CGAALParser, Parser, TypedValue};
use cgaal_engine::game_structure::GameStructure;
use cgaal_engine::game_structure::State;

use std::io;
use std::io::Write;

pub struct CGAALInterpreter<G: GameStructure> {
    pub cgs: G,
    pub curr_state: State,
}

impl<G: GameStructure + Clone> CGAALInterpreter<G> {
    pub fn new(cgs: G) -> Self {
        let initial_state = cgs.initial_state_index();
        Self {
            cgs,
            curr_state: initial_state,
        }
    }

    pub fn run(&mut self) {
        let mut parser = CGAALParser::new();

        loop {
            let mut buffer = String::new();
            io::stdin()
                .read_line(&mut buffer)
                .expect("Couldn't read from stdin");


            let cgs = &self.cgs.clone();
            let state = self.curr_state;
            let parse_res = parser.parse(&buffer);
            if let Ok(op) = parse_res {
                match self.resolve(cgs, state, op) {
                    Ok(state) => {
                        self.curr_state = state;
                    }
                    Err(err) => {
                        io::stdout()
                            .write_all(err.as_bytes())
                            .expect("Couldn't write to stdout");
                    }
                }
            } else if let Err(err) = parse_res {
                io::stdout().write_all(
                    err.as_bytes()
                ).expect("Couldn't write to stdout");
            }
            io::stdout().flush().expect("Couldn't write to stdout");
        }
    }

    pub fn resolve(
        &mut self,
        cgs: &G,
        state: State,
        op: Op,
    ) -> Result<State, String> {
        match op {
            // Do nothing
            Op::Skip => Ok(state),
            // Do a transition given a move vector
            Op::Move { moves } => Ok(cgs.transitions(state, moves)),
            // Print all players with human readable names
            Op::DisplayPlayers => {
                println!("[");
                for i in 0..cgs.max_player() {
                    println!(" {}: {}", i, cgs.player_name(i));
                }
                println!("]");
                Ok(state)
            }
            Op::DisplayPlayer { player } => {
                let mut index = None;

                if let TypedValue::Player(Some(name), _) = player {
                    for i in 0..cgs.max_player() {
                        if name.eq(&cgs.player_name(i)) {
                            index = Some(i);
                        }
                    }
                } else if let TypedValue::Player(None, Some(i)) = player {
                    index = Some(i);
                }

                if let Some(id) = index {
                    let move_count = cgs.move_count(state)[id];
                    println!("Player: {}", cgs.player_name(id));
                    print!("Actions: ");
                    for i in 0..move_count {
                        print!("{}, ", cgs.action_name(state, id, i));
                    }
                    return Ok(state);
                }
                Err("Got an unexpected value".to_string())
            }
        }
    }
}
