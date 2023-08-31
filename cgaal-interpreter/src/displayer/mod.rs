use crate::parser::TypedValue;
use cgaal_engine::game_structure::{Action, GameStructure, Proposition};

pub enum ViewType {
    AllActions {
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
    Action {
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
        todo!()
    }
}

impl ICGAAL {
    pub fn new() -> Self {
        Self {}
    }
}
