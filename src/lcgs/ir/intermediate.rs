use std::collections::HashSet;

use crate::lcgs::ast;
use crate::lcgs::ast::{
    BinaryOpKind, ConstDecl, Decl, DeclKind, Expr, ExprKind, OwnedIdentifier, Root, UnaryOpKind,
};
use crate::lcgs::ast::ExprKind::Number;
use crate::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};

pub struct Player {
    name: String,
    actions: Vec<SymbolIdentifier>,
}

impl Player {
    pub fn new(name: &str) -> Player {
        Player {
            name: name.to_string(),
            actions: vec![],
        }
    }
}

#[derive(Default)]
pub struct IntermediateLCGS {
    symbols: SymbolTable,
    constants: Vec<SymbolIdentifier>,
    labels: Vec<SymbolIdentifier>,
    templates: Vec<SymbolIdentifier>,
    vars: Vec<SymbolIdentifier>,
    var_changes: Vec<SymbolIdentifier>,
    player_decls: Vec<Decl>,
    players: Vec<Player>,
}

impl IntermediateLCGS {
    pub fn create(root: Root) -> Result<IntermediateLCGS, ()> {
        let mut ilcgs: IntermediateLCGS = Default::default();

        ilcgs.register_global_decls(root);
        ilcgs.register_player_decls();

        return Ok(ilcgs);
    }

    fn register_global_decls(&mut self, root: Root) {
        for decl in root.decls {
            self.register_global_decl(decl);
        }
    }

    fn register_global_decl(&mut self, decl: ast::Decl) -> Result<(), ()> {
        let global = Owner::Global;
        // TODO Check re-declarations
        match &decl.kind {
            DeclKind::Const(mut cons) => {
                // We can evaluate constants immediately as constants can only refer to
                // other constants that are above them in the program.
                let result = Evaluator::new(&self.symbols, Owner::Global).eval(&cons.definition)?;
                cons.definition = Expr {
                    kind: Number(result),
                };
                let sym_id = global.symbol_id(&cons.name.name);
                self.symbols.insert(&sym_id, decl);
                self.constants.push(sym_id);
            }
            DeclKind::Label(label) => {
                let sym_id = global.symbol_id(&label.name.name);
                self.symbols.insert(&sym_id, decl);
                self.labels.push(sym_id);
            }
            DeclKind::StateVar(var) => {
                let sym_id = global.symbol_id(&var.name.name);
                self.symbols.insert(&sym_id, decl);
                self.vars.push(sym_id);
            }
            DeclKind::StateVarChange(var_change) => {
                let sym_id = global.symbol_id(&var_change.name.name);
                self.symbols.insert(&sym_id, decl);
                self.var_changes.push(sym_id);
            }
            DeclKind::Player(player) => {
                // We handle player declarations later
                self.player_decls.push(decl);
            }
            DeclKind::Template(temp) => {
                let sym_id = global.symbol_id(&temp.name.name);
                self.symbols.insert(&sym_id, decl);
                self.templates.push(sym_id);
            }
            _ => panic!("Not a global declaration. Parser must have failed."), // Not a global decl
        }
        Ok(())
    }

    fn register_player_decls(&mut self) {
        for player_decl in &self.player_decls {
            if let DeclKind::Player(player_decl) = &player_decl.kind {
                let mut player = Player::new(&player_decl.name.name);
                let temp = self
                    .symbols
                    .get_(&Owner::Global, &player_decl.template.name)
                    .unwrap()
                    .expect("Unknown template"); // TODO Use custom error
                if let DeclKind::Template(temp_decl) = &temp.declaration.kind {
                    for decl in &temp_decl.decls {
                        self.register_player_decl(&mut player, decl);
                    }
                } else {
                    panic!("Not a template"); // TODO Use custom error
                }
                self.players.push(player);
            } else {
                panic!("A non-PlayerDecl got into our player_decl list.")
            }
        }
    }

    fn register_player_decl(&mut self, player: &mut Player, decl: &Decl) {
        let scope_owner = Owner::Player(player.name.clone());
        match &decl.kind {
            DeclKind::Label(_) => {}
            DeclKind::StateVar(_) => {}
            DeclKind::StateVarChange(_) => {}
            DeclKind::Transition(_) => {}
            _ => panic!("Not a declaration allowed in templates. Parser must have failed."),
        }
    }
}
