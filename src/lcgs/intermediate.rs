use crate::lcgs::ast;
use crate::lcgs::symbol_table::{SymbolTable, Owner, SymbolIdentifier};
use crate::lcgs::ast::{DeclKind, Decl, ConstDecl, Root};
use std::collections::HashSet;

#[derive(Default)]
pub struct IntermediateLCGS {
    symbols: SymbolTable,
    constants: HashSet<SymbolIdentifier>,
    labels: HashSet<SymbolIdentifier>,
    templates: HashSet<SymbolIdentifier>,
    vars: HashSet<SymbolIdentifier>,
    var_changes: HashSet<SymbolIdentifier>,
    player_decls: Vec<Decl>,
}

impl IntermediateLCGS {
    pub fn create(root: Root) -> Result<IntermediateLCGS, ()> {
        let mut ilcgs: IntermediateLCGS = Default::default();

        ilcgs.register_global_decls(root);
        ilcgs.register_player_decls();

        return Ok(ilcgs)
    }

    fn register_global_decls(&mut self, root: Root) {
        for decl in root.decls {
            ilcgs.register_global_decl(decl);
        }
    }

    fn register_global_decl(&mut self, decl: ast::Decl) -> Result<(), ()> {
        let global = Owner::Global;
        // TODO Check redeclarations
        match &decl.kind {
            DeclKind::Const(cons) => {
                let sym_id = global.symbol_id(&cons.name.name);
                self.symbols.insert(&sym_id, decl);
                self.constants.insert(sym_id);
            },
            DeclKind::Label(label) => {
                let sym_id = global.symbol_id(&label.name.name);
                self.symbols.insert(&sym_id, decl);
                self.labels.insert(sym_id);
            },
            DeclKind::StateVar(var) => {
                let sym_id = global.symbol_id(&var.name.name);
                self.symbols.insert(&sym_id, decl);
                self.vars.insert(sym_id);
            },
            DeclKind::StateVarChange(var_change) => {
                let sym_id = global.symbol_id(&var_change.name.name);
                self.symbols.insert(&sym_id, decl);
                self.var_changes.insert(sym_id);
            },
            DeclKind::Player(player) => {
                // We handle player declarations later
                self.player_decls.push(decl);
            },
            DeclKind::Template(temp) => {
                let sym_id = global.symbol_id(&temp.name.name);
                self.symbols.insert(&sym_id, decl);
                self.templates.insert(sym_id);
            },
            _ => panic!("Not a global declaration. Parser must have failed."), // Not a global decl
        }
        Ok(())
    }

    fn register_player_decls(&mut self) {
        for player_decl in self.player_decls {
            if let DeclKind::Player(player_decl) = player_decl.kind {
                unimplemented!()
            } else {
                panic!("A non-PlayerDecl got into our player_decl list.")
            }
        }
    }
}