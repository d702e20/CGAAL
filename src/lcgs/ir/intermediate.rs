use std::collections::HashSet;

use crate::lcgs::ast;
use crate::lcgs::ast::ExprKind::Number;
use crate::lcgs::ast::{
    BinaryOpKind, ConstDecl, Decl, DeclKind, Expr, ExprKind, Identifier, Root, UnaryOpKind,
};
use crate::lcgs::ir::eval::Evaluator;
use crate::lcgs::ir::symbol_checker::{CheckMode, SymbolChecker};
use crate::lcgs::ir::symbol_table::Owner::Global;
use crate::lcgs::ir::symbol_table::{Owner, Symbol, SymbolIdentifier, SymbolTable};
use std::borrow::BorrowMut;
use std::cell::RefCell;

/// A struct that holds information about players for the intermediate representation
/// of the lazy game structure
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

    /// Helper function to quickly turn a player into an [Owner]
    pub fn to_owner(&self) -> Owner {
        Owner::Player(self.name.clone())
    }
}

/// An [IntermediateLCGS] is created from processing an AST and checking the validity of the
/// declarations.
pub struct IntermediateLCGS {
    symbols: SymbolTable,
    labels: Vec<SymbolIdentifier>,
    vars: Vec<SymbolIdentifier>,
    players: Vec<Player>,
}

impl IntermediateLCGS {
    /// Create an [IntermediateLCGS] from an AST root. All declarations in the resulting
    /// [IntermediateLCGS] are symbol checked and type checked.
    pub fn create(mut root: Root) -> Result<IntermediateLCGS, ()> {
        let mut symbols = SymbolTable::new();

        // Register global decls. Then check and optimize them
        let players = register_decls(&mut symbols, root)?;
        check_and_optimize_decls(&mut symbols)?;

        // Collect all symbol names that will be relevant for the game structure
        let labels = fetch_decls(&symbols, |_, rf_decl| {
            matches!(rf_decl.borrow().declaration.kind, DeclKind::Label(_))
        });
        let vars = fetch_decls(&symbols, |_, rf_decl| {
            matches!(rf_decl.borrow().declaration.kind, DeclKind::StateVar(_))
        });

        let ilcgs = IntermediateLCGS {
            symbols,
            labels,
            vars,
            players,
        };

        return Ok(ilcgs);
    }
}

/// Helper function to find symbols in the given [SymbolTable] that satisfies the given
/// predicate.
fn fetch_decls<F>(symbols: &SymbolTable, pred: F) -> Vec<SymbolIdentifier>
where
    F: Fn(&SymbolIdentifier, &RefCell<Symbol>) -> bool,
{
    symbols
        .iter()
        .filter_map(|(symb, rf_decl)| {
            if pred(symb, rf_decl) {
                Some(symb.clone())
            } else {
                None
            }
        })
        .collect()
}

/// Registers all declarations from the root in the symbol table. Constants are optimized to
/// numbers immediately. On success, a vector of [Player]s is returned with information
/// about players and the names of their actions.
fn register_decls(symbols: &mut SymbolTable, root: Root) -> Result<Vec<Player>, ()> {
    let mut player_decls = vec![];
    let mut player_names = HashSet::new();

    // Register global declarations.
    // Constants are evaluated immediately.
    // Players are put in a separate vector and handled afterwards.
    // Symbol table is given ownership of the declarations.
    for decl in root.decls {
        match &decl.kind {
            DeclKind::Const(cons) => {
                // We can evaluate constants immediately as constants can only
                // refer to other constants that are above them in the program.
                // If they don't reduce to a single number, then the SymbolChecker
                // produces an error.
                let result = SymbolChecker::new(&symbols, Owner::Global, CheckMode::Const)
                    .check(&cons.definition)?;
                debug_assert!(matches!(result.kind, ExprKind::Number(_)));
                let name = cons.name.name().to_string();
                // Construct a resolved constant decl
                let decl = Decl {
                    kind: DeclKind::Const(Box::new(ConstDecl {
                        name: Identifier::Resolved {
                            owner: Owner::Global,
                            name: name.clone(),
                        },
                        definition: result,
                    })),
                };
                if symbols.insert(&Owner::Global, &name, decl).is_some() {
                    panic!("Constant '{}' is already declared.", &name); // TODO Use custom error
                }
            }
            DeclKind::Label(_)
            | DeclKind::StateVar(_)
            | DeclKind::Template(_) => {
                // All of the above declaration kinds can simply be inserted into the symbol table
                let name = decl.kind.ident().name().to_string();
                if symbols.insert(&Owner::Global, &name, decl).is_some() {
                    panic!("Symbol '{}' is already declared.", &name); // TODO Use custom error
                }
            }
            DeclKind::Player(player) => {
                // We handle player declarations later
                if !player_names.insert(player.name.name().to_string()) {
                    panic!("Player '{}' is already declared", &player.name.name());
                    // TODO Use custom error
                }
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
                // Go through each declaration in the template and register a clone of it
                // that is owned by the given player
                let scope_owner = player.to_owner();
                for decl in template.decls {
                    match &decl.kind {
                        DeclKind::Label(_)
                        | DeclKind::StateVar(_) => {
                            // The above declarations can simply be inserted into the symbol table
                            let name = decl.kind.ident().name().to_string();
                            if symbols.insert(&scope_owner, &name, decl.clone()).is_some() {
                                panic!("Symbol '{}.{}' is already declared.", &scope_owner, &name);
                            };
                        }
                        DeclKind::Transition(tran) => {
                            // Transitions are inserted in the symbol table, but their name
                            // is also stored in the player.actions so they can easily be found
                            // later when run.
                            let name = tran.name.name().to_string();
                            if symbols.insert(&scope_owner, &name, decl.clone()).is_some() {
                                panic!("Action '{}.{}' is already declared.", &scope_owner, &name);
                            };
                            player.actions.push(scope_owner.symbol_id(&name));
                        }
                        _ => panic!(
                            "Not a declaration allowed in templates. Parser must have failed."
                        ),
                    }
                }
            } else {
                panic!("'{}' is not a template.", decl.kind.ident().name()); // TODO Use custom error
            }

            // The player is done. We can now register the player declaration.
            players.push(player);
            let name = player_decl.name.name().to_string();
            symbols.insert(&Owner::Global, &name, decl);
        } else {
            panic!("A non-PlayerDecl got into this vector");
        }
    }
    Ok(players)
}

/// Reduces the declarations in a [SymbolTable] to a more compact version, if possible.
/// Validity of identifiers are also checked and resolved.
fn check_and_optimize_decls(symbols: &SymbolTable) -> Result<(), ()> {
    for (symb_id, rc_symb) in symbols {
        // Create resolved name
        let SymbolIdentifier { owner, name } = symb_id;
        let resolved_name = Identifier::Resolved {
            owner: owner.clone(),
            name: name.clone(),
        };

        // Reduce the declaration's expression(s)
        let mut symb = rc_symb.borrow_mut();
        match symb.declaration.kind.borrow_mut() {
            DeclKind::Label(label) => {
                label.name = resolved_name;
                label.condition =
                    SymbolChecker::new(symbols, owner.clone(), CheckMode::LabelOrTransition)
                        .check(&label.condition)?;
            }
            DeclKind::StateVar(var) => {
                var.name = resolved_name;
                // Both initial value, min, and max are expected to be constant
                let checker = SymbolChecker::new(symbols, owner.clone(), CheckMode::Const);
                var.initial_value = checker.check(&var.initial_value)?;
                var.range.min = checker.check(&var.range.min)?;
                var.range.max = checker.check(&var.range.max)?;
                var.next_value =
                    SymbolChecker::new(symbols, owner.clone(), CheckMode::StateVarUpdate)
                        .check(&var.next_value)?;
            }
            DeclKind::Transition(tran) => {
                tran.name = resolved_name;
                tran.condition =
                    SymbolChecker::new(symbols, owner.clone(), CheckMode::LabelOrTransition)
                        .check(&tran.condition)?;
            }
            DeclKind::Player(player) => {
                player.name = resolved_name;
            }
            DeclKind::Template(template) => {
                template.name = resolved_name;
            }
            DeclKind::Const(_) => {} // Needs no further reduction
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
        // Check if the correct symbols are inserted into the symbol table
        let input = br"
        const max_health = 100;
        player anna = gamer;
        player bob = gamer;

        template gamer
            health : [0 .. max_health] init max_health;
            health' = health - 1;

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
