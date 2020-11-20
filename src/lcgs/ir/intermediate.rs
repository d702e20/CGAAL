use std::collections::HashSet;

use crate::lcgs::ast;
use crate::lcgs::ast::ExprKind::Number;
use crate::lcgs::ast::{
    BinaryOpKind, ConstDecl, Decl, DeclKind, Expr, ExprKind, OwnedIdentifier, Root, UnaryOpKind,
};
use crate::lcgs::ir::eval::Evaluator;
use crate::lcgs::ir::symbol_table::Owner::Global;
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
        let players = IntermediateLCGS::register_decls(&mut symbols, root)?;
        let ilcgs = IntermediateLCGS { symbols, players };

        return Ok(ilcgs);
    }

    fn register_decls(symbols: &mut SymbolTable, root: Root) -> Result<Vec<Player>, ()> {

        let mut player_decls = vec![];

        // Register global declarations. Constants are evaluated immediately
        // Players are handled afterwards
        for decl in root.decls {
            match &decl.kind {
                DeclKind::Const(cons) => {
                    // We can evaluate constants immediately as constants can only refer to
                    // other constants that are above them in the program.
                    let result = Evaluator::new(&symbols, &Owner::Global).eval(&cons.definition)?;
                    let evaluated = Decl {
                        kind: DeclKind::Const(Box::new(ConstDecl {
                            name: cons.name.clone(),
                            definition: Expr { kind: Number(result) },
                        }))
                    };
                    symbols.insert(&Owner::Global, &cons.name.name.clone(), evaluated);
                }
                DeclKind::Label(_) | DeclKind::StateVar(_) | DeclKind::StateVarChange(_) | DeclKind::Template(_) => {
                    symbols.insert(&Owner::Global, &decl.kind.ident().name.clone(), decl);
                }
                DeclKind::Player(player) => {
                    // We handle player declarations later
                    player_decls.push(*player.clone());
                }
                _ => panic!("Not a global declaration. Parser must have failed."), // Not a global decl
            }
        }

        // Register player declarations. Here we clone the declarations since multiple
        // players can use the same template
        let mut players = vec![];
        for player_decl in player_decls {
            let mut player = Player::new(&player_decl.name.name);
            let template_decl = symbols
                .get(&Owner::Global, &player_decl.template.name)
                .expect("Unknown template") // TODO Use custom error
                .declaration.clone();
            if let DeclKind::Template(template) = template_decl.kind {
                for decl in template.decls {
                    let scope_owner = player.to_owner();
                    match &decl.kind {
                        DeclKind::Label(_) | DeclKind::StateVar(_) | DeclKind::StateVarChange(_) => {
                            symbols.insert(&scope_owner, &decl.kind.ident().name.clone(), decl.clone());
                        }
                        DeclKind::Transition(tran) => {
                            symbols.insert(&scope_owner, &tran.name.name.clone(), decl.clone());
                            player.actions.push(scope_owner.symbol_id(&tran.name.name));
                        }
                        _ => panic!("Not a declaration allowed in templates. Parser must have failed."),
                    }
                }
            } else {
                panic!("Not a template"); // TODO Use custom error
            }
            players.push(player);
        }
        Ok(players)
    }
}

#[cfg(test)]
mod test {
    use crate::lcgs::parse::parse_lcgs;
    use crate::lcgs::ir::intermediate::IntermediateLCGS;
    use crate::lcgs::ir::symbol_table::Owner;

    #[test]
    #[ignore]
    fn test_symbol_01() {
        let input = br"
        const max_health = 1;
        player anna = gamer;
        player bob = gamer;

        template gamer
            health : [0 .. max_health] init max_health;
            label alive = health > 0;
        endtemplate
        ";
        let lcgs = IntermediateLCGS::create(parse_lcgs(input).unwrap()).unwrap();
        assert_eq!(lcgs.symbols.len(), 6);
        assert!(lcgs.symbols.get(&Owner::Global, "max_health").is_some());
        assert!(lcgs.symbols.get(&Owner::Global, "gamer").is_some());
        assert!(lcgs.symbols.get(&Owner::Player("anna".to_string()), "health").is_some());
        assert!(lcgs.symbols.get(&Owner::Player("anna".to_string()), "alive").is_some());
        assert!(lcgs.symbols.get(&Owner::Player("bob".to_string()), "health").is_some());
        assert!(lcgs.symbols.get(&Owner::Player("bob".to_string()), "alive").is_some());
    }
}
