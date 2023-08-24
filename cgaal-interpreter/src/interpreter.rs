use crate::op::{run, Op};
use crate::parser::{CGAALParser, Parser};
use cgaal_engine::game_structure::EagerGameStructure;
use cgaal_engine::game_structure::GameStructure;
use cgaal_engine::game_structure::State;
use std::borrow::{Borrow, BorrowMut};

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
            cgs: cgs.clone(),
            curr_state: initial_state,
        }
    }

    pub fn new_empty() -> Self {
        Self {
            cgs: None,
            curr_state: None,
        }
    }

    pub fn run(&mut self) {
        loop {
            let mut buffer = String::new();
            io::stdin()
                .read_line(&mut buffer)
                .expect("Couldn't read from stdin");
            match run(
                self.cgs.borrow_mut(),
                self.curr_state,
                CGAALParser::parse(&*buffer),
            ) {
                Ok(state) => {
                    self.curr_state = state;
                }
                Err(err) => {
                    io::stdout()
                        .write(err.as_bytes())
                        .expect("Couldn't write to stdout");
                }
            }
        }
    }
}
