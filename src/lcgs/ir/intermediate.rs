use std::collections::HashSet;

use crate::lcgs::ast;
use crate::lcgs::ast::ExprKind::Number;
use crate::lcgs::ast::{
    BinaryOpKind, ConstDecl, Decl, DeclKind, Expr, ExprKind, Identifier, Root, UnaryOpKind,
};
use crate::lcgs::ir::eval::Evaluator;
use crate::lcgs::ir::reducer::{ReduceMode, Reducer};
use crate::lcgs::ir::symbol_table::Owner::Global;
use crate::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};
use std::borrow::BorrowMut;

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

    pub fn to_owner(&self) -> Owner {
        Owner::Player(self.name.clone())
    }
}

#[derive(Default)]
pub struct IntermediateLCGS {
    symbols: SymbolTable,
    // constants: Vec<SymbolIdentifier>,
    // labels: Vec<SymbolIdentifier>,
    // templates: Vec<SymbolIdentifier>,
    // vars: Vec<SymbolIdentifier>,
    // var_changes: Vec<SymbolIdentifier>,
    players: Vec<Player>,
}

impl IntermediateLCGS {
    pub fn create(mut root: Root) -> Result<IntermediateLCGS, ()> {
        // Register global decls, evaluate constants immediately
        // Handle players and register their decls from template

        let mut symbols = SymbolTable::new();
        let players = register_decls(&mut symbols, root)?;
        reduce_decls(&mut symbols)?;
        let ilcgs = IntermediateLCGS { symbols, players };

        return Ok(ilcgs);
    }
}

fn register_decls(symbols: &mut SymbolTable, root: Root) -> Result<Vec<Player>, ()> {
    let mut player_decls = vec![];

    // Register global declarations.
    // Constants are evaluated immediately.
    // Players are handled afterwards.
    // Symbol table is given ownership of the declarations.
    for decl in root.decls {
        match &decl.kind {
            DeclKind::Const(cons) => {
                // We can reduce (evaluate) constants immediately as constants can only
                // refer to other constants that are above them in the program.
                // If they don't reduce to a single number, then the Reducer produces
                // an error.
                let result = Reducer::new(&symbols, Owner::Global, ReduceMode::Const)
                    .reduce(&cons.definition)?;
                let name = cons.name.name().to_string();
                debug_assert!(matches!(result.kind, ExprKind::Number(_)));
                // Construct resolved and reduced decl
                let decl = Decl {
                    kind: DeclKind::Const(Box::new(ConstDecl {
                        name: Identifier::Resolved {
                            owner: Owner::Global,
                            name: name.clone(),
                        },
                        definition: result,
                    })),
                };
                symbols.insert(&Owner::Global, &name, decl);
            }
            DeclKind::Label(_)
            | DeclKind::StateVar(_)
            | DeclKind::StateVarChange(_)
            | DeclKind::Template(_) => {
                let name = decl.kind.ident().name().to_string();
                symbols.insert(&Owner::Global, &name, decl);
            }
            DeclKind::Player(player) => {
                // We handle player declarations later
                player_decls.push(decl);
            }
            _ => panic!("Not a global declaration. Parser must have failed."), // Not a global decl
        }
    }

    // Register player declarations. Here we clone the declarations since multiple
    // players can use the same template
    let mut players = vec![];
    for decl in player_decls {
        if let DeclKind::Player(player_decl) = &decl.kind {
            let mut player = Player::new(&player_decl.name.name());
            let template_decl = symbols
                .get(&Owner::Global, &player_decl.template.name())
                .expect("Unknown template") // TODO Use custom error
                .borrow()
                .declaration
                .clone();
            if let DeclKind::Template(template) = template_decl.kind {
                for decl in template.decls {
                    let scope_owner = player.to_owner();
                    match &decl.kind {
                        DeclKind::Label(_)
                        | DeclKind::StateVar(_)
                        | DeclKind::StateVarChange(_) => {
                            symbols.insert(
                                &scope_owner,
                                &decl.kind.ident().name().clone(),
                                decl.clone(),
                            );
                        }
                        DeclKind::Transition(tran) => {
                            symbols.insert(&scope_owner, &tran.name.name().clone(), decl.clone());
                            player
                                .actions
                                .push(scope_owner.symbol_id(&tran.name.name()));
                        }
                        _ => panic!(
                            "Not a declaration allowed in templates. Parser must have failed."
                        ),
                    }
                }
            } else {
                panic!("Not a template"); // TODO Use custom error
            }
            players.push(player);
            let name = player_decl.name.name().to_string();
            symbols.insert(&Owner::Global, &name, decl);
        } else {
            panic!("A non-PlayerDecl got into this vector");
        }
    }
    Ok(players)
}

fn reduce_decls(symbols: &SymbolTable) -> Result<(), ()> {
    for (symb_id, rc_symb) in symbols {
        let SymbolIdentifier { owner, .. } = symb_id;
        let mut symb = rc_symb.borrow_mut();
        match symb.declaration.kind.borrow_mut() {
            DeclKind::Label(label) => {
                label.condition =
                    Reducer::new(symbols, owner.clone(), ReduceMode::LabelOrTransition)
                        .reduce(&label.condition)?;
            }
            DeclKind::StateVar(var) => {
                let reducer = Reducer::new(symbols, owner.clone(), ReduceMode::Const);
                var.initial_value = reducer.reduce(&var.initial_value)?;
                var.range.min = reducer.reduce(&var.range.min)?;
                var.range.max = reducer.reduce(&var.range.max)?;
            }
            DeclKind::StateVarChange(var_change) => {
                var_change.next_value =
                    Reducer::new(symbols, owner.clone(), ReduceMode::StateVarChange)
                        .reduce(&var_change.next_value)?;
            }
            DeclKind::Transition(tran) => {
                tran.condition =
                    Reducer::new(symbols, owner.clone(), ReduceMode::LabelOrTransition)
                        .reduce(&tran.condition)?;
            }
            _ => {} // Needs no further reduction
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::lcgs::ir::intermediate::IntermediateLCGS;
    use crate::lcgs::ir::symbol_table::Owner;
    use crate::lcgs::parse::parse_lcgs;

    #[test]
    fn test_symbol_01() {
        let input = br"
        const max_health = 100;
        player anna = gamer;
        player bob = gamer;

        template gamer
            health : [0 .. max_health] init max_health;

            label alive = health > 0;

            [wait] 1;
            [shoot] health > 0;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(lcgs.symbols.len(), 12);
        assert!(lcgs.symbols.get(&Owner::Global, "max_health").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "anna").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "bob").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "gamer").is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "health")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "alive")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "wait")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("anna".to_string()), "shoot")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "health")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "alive")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "wait")
            .is_some());
        assert!(lcgs
            .symbols
            .get(&Owner::Player("bob".to_string()), "shoot")
            .is_some());
    }
}
