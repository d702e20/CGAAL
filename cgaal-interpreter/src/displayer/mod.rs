use crate::parser::TypedValue;
use cgaal_engine::game_structure::{Action, GameStructure, Proposition};

pub enum ViewType {
    AllMoves {
        players: Vec<ViewType>,
    },
    AllLabels {
        labels: Vec<ViewType>,
    },
    AllPlayers {
        players: Vec<ViewType>,
    },
    Player {
        index: usize,
        name: String,
        actions: Vec<ViewType>,
    },
    Label {
        index: Proposition,
        name: String,
    },
    Move {
        index: usize,
        name: String,
    },
}

pub trait Displayer {
    fn display(&self, view: ViewType);
}

pub struct ICGAAL {}

impl Displayer for ICGAAL {
    fn display(&self, view: ViewType) {
        match view {
            ViewType::AllMoves { players } => {
                for player in players {
                    if let ViewType::Player {
                        index,
                        name,
                        actions,
                    } = player
                    {
                        print!("{}: ", name);
                        for action in actions {
                            if let ViewType::Move { index, name, .. } = action {
                                print! {"{} ", name}
                            }
                        }
                        println!();
                    }
                }
            }
            ViewType::AllLabels { .. } => {}
            ViewType::AllPlayers { .. } => {}
            ViewType::Player { .. } => {}
            ViewType::Label { .. } => {}
            ViewType::Move { .. } => {}
        }
    }
}

impl ICGAAL {
    pub fn new() -> Self {
        Self {}
    }
}
