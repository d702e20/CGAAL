use crate::op::Op;
use crate::parser::{CGAALParser, Parser, TypedValue};
use cgaal_engine::game_structure::GameStructure;
use cgaal_engine::game_structure::State;
use std::collections::HashMap;

use crate::displayer::{Displayer, ViewType};
use std::io;
use std::io::Write;

pub struct CGAALInterpreter<'a, G: GameStructure> {
    pub cgs: G,
    pub curr_state: State,
    pub symbol_table: HashMap<String, TypedValue>,
    displayer: &'a dyn Displayer,
}

impl<'a, G: GameStructure + Clone> CGAALInterpreter<'a, G> {
    pub fn new(cgs: G, displayer: &'a dyn Displayer) -> Self {
        let initial_state = cgs.initial_state_index();

        // Add all players and their names to the symbol table
        let mut symbol_table = HashMap::new();
        for i in 0..cgs.max_player() {
            let player_name = cgs.player_name(i);
            symbol_table.insert(
                player_name.clone(),
                TypedValue::Player(Some(player_name), Some(i)),
            );
        }

        Self {
            cgs,
            curr_state: initial_state,
            symbol_table,
            displayer,
        }
    }

    pub fn run(&mut self) {
        let mut parser = CGAALParser::new();

        loop {
            let mut buffer = String::new();
            io::stdin()
                .read_line(&mut buffer)
                .expect("Couldn't read from stdin");

            let parse_res = parser.parse(&buffer);
            if let Ok(op) = parse_res {
                match self.resolve(op) {
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
                io::stdout()
                    .write_all(err.as_bytes())
                    .expect("Couldn't write to stdout");
            }
            io::stdout().flush().expect("Couldn't write to stdout");
        }
    }

    pub fn resolve(&mut self, op: Op) -> Result<State, String> {
        match op {
            // Do nothing
            Op::Skip => Ok(self.curr_state),
            // Do a transition given a move vector
            Op::Move { moves } => Ok(self.cgs.transitions(self.curr_state, moves)),
            Op::DisplayAll { value } => {
                match value {
                    TypedValue::Move(_, _) => match self.construct_players_view() {
                        Ok(players) => self.displayer.display(ViewType::AllMoves { players }),
                        Err(err) => return Err(err),
                    },
                    TypedValue::Player(_, _) => match self.construct_players_view() {
                        Ok(players) => self.displayer.display(ViewType::AllPlayers { players }),
                        Err(err) => return Err(err),
                    },
                    TypedValue::Label(_, _) => {
                        let mut labels = Vec::new();
                        self.cgs.labels(self.curr_state).iter().for_each(|index| {
                            labels.push(ViewType::Label {
                                index: *index,
                                name: self.cgs.label_name(*index),
                            })
                        });
                        self.displayer.display(ViewType::AllLabels { labels });
                    }
                }
                Ok(self.curr_state)
            }
            Op::Display { value } => Ok(self.curr_state),
        }
    }

    fn symbol_table_get(&self, identifier: &str) -> Result<TypedValue, String> {
        match self.symbol_table.get(identifier) {
            None => Err(format!("The identifier [{}] does not exist", identifier)),
            Some(value) => Ok(value.clone()),
        }
    }

    fn construct_players_view(&self) -> Result<Vec<ViewType>, String> {
        let move_count = self.cgs.move_count(self.curr_state);
        let mut players = Vec::new();
        for i in 0..self.cgs.max_player() {
            let mut actions = Vec::new();
            for j in 0..move_count[i] {
                actions.push(ViewType::Move {
                    index: j,
                    name: self.cgs.action_name(self.curr_state, i, j),
                })
            }
            players.push(ViewType::Player {
                index: i,
                name: self.cgs.player_name(i),
                actions,
            });
        }
        Ok(players)
    }
}
