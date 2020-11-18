use crate::lcgs::ast;
use crate::lcgs::symbol_table::{SymbolTable, Owner, SymbolIdentifier};
use crate::lcgs::ast::{DeclKind, Decl, ConstDecl, Root, Expr, ExprKind, BinaryOpKind, UnaryOpKind, OwnedIdentifier};
use std::collections::HashSet;

#[derive(Default)]
pub struct IntermediateLCGS {
    symbols: SymbolTable,
    constants: Vec<SymbolIdentifier>,
    labels: Vec<SymbolIdentifier>,
    templates: Vec<SymbolIdentifier>,
    vars: Vec<SymbolIdentifier>,
    var_changes: Vec<SymbolIdentifier>,
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
            self.register_global_decl(decl);
        }
    }

    fn register_global_decl(&mut self, decl: ast::Decl) -> Result<(), ()> {
        let global = Owner::Global;
        // TODO Check redeclarations
        match &decl.kind {
            DeclKind::Const(cons) => {
                let sym_id = global.symbol_id(&cons.name.name);
                self.symbols.insert(&sym_id, decl);
                self.constants.push(sym_id);
            },
            DeclKind::Label(label) => {
                let sym_id = global.symbol_id(&label.name.name);
                self.symbols.insert(&sym_id, decl);
                self.labels.push(sym_id);
            },
            DeclKind::StateVar(var) => {
                let sym_id = global.symbol_id(&var.name.name);
                self.symbols.insert(&sym_id, decl);
                self.vars.push(sym_id);
            },
            DeclKind::StateVarChange(var_change) => {
                let sym_id = global.symbol_id(&var_change.name.name);
                self.symbols.insert(&sym_id, decl);
                self.var_changes.push(sym_id);
            },
            DeclKind::Player(player) => {
                // We handle player declarations later
                self.player_decls.push(decl);
            },
            DeclKind::Template(temp) => {
                let sym_id = global.symbol_id(&temp.name.name);
                self.symbols.insert(&sym_id, decl);
                self.templates.push(sym_id);
            },
            _ => panic!("Not a global declaration. Parser must have failed."), // Not a global decl
        }
        Ok(())
    }

    fn register_player_decls(&mut self) {
        for player_decl in &self.player_decls {
            if let DeclKind::Player(player_decl) = &player_decl.kind {
                unimplemented!()
            } else {
                panic!("A non-PlayerDecl got into our player_decl list.")
            }
        }
    }
}

struct Evaluator<'a> {
    symbols: &'a SymbolTable,
    scope_owner: Owner,
}

impl<'a> Evaluator<'a> {
    pub fn new(symbols: &'a SymbolTable, scope_owner: Owner) -> Evaluator<'a> {
        Evaluator {
            symbols,
            scope_owner,
        }
    }

    pub fn eval(&self, expr: Expr) -> Result<i32, ()> {
        match expr.kind {
            ExprKind::Number(n) => Ok(n),
            ExprKind::OwnedIdent(id) => self.eval_ident(*id),
            ExprKind::UnaryOp(op, e) => self.eval_unop(op, *e),
            ExprKind::BinaryOp(op, e1, e2) => self.eval_binop(op, *e1, *e2),
            ExprKind::TernaryIf(c, e1, e2) => self.eval_if(*c, *e1, *e2),
        }
    }

    fn eval_ident(&self, id: OwnedIdentifier) -> Result<i32, ()> {
        // Owner may be omitted. If omitted, we assume it is the scope owner, unless such thing
        // does not exist, then we assume it's global. If we still can't find it, we have an error.
        let OwnedIdentifier { owner, name } = id;
        let symb = if let Some(player_name) = owner {
            let owner = Owner::Player(player_name);
            self.symbols.get_(&owner, &name)
                .expect("Unknown player") // TODO Use custom error
                .expect("Unknown identifier") // TODO Use custom error
        } else {
            self.symbols.get_(&self.scope_owner, &name).unwrap()
                .or_else(|| self.symbols.get_(&Owner::Global, &name).unwrap())
                .expect("Unknown identifier, neither declared locally or globally") // TODO Use custom error
        };

        match &symb.declaration.kind {
            DeclKind::Const(_) => unimplemented!(), // TODO
            DeclKind::Label(_) => unimplemented!(), // TODO Depends on context
            DeclKind::StateVar(_) => unimplemented!(), // TODO Depends on context
            DeclKind::Transition(_) => unimplemented!(), // TODO Depends on context
            _ => Err(()),
        }
    }

    fn eval_unop(&self, op: UnaryOpKind, e: Expr) -> Result<i32, ()> {
        let res = self.eval(e)?;
        Ok(op.as_fun()(res))
    }

    fn eval_binop(&self, op: BinaryOpKind, e1: Expr, e2: Expr) -> Result<i32, ()> {
        let res1 = self.eval(e1)?;
        let res2 = self.eval(e2)?;
        Ok(op.as_fn()(res1, res2))
    }
    fn eval_if(&self, c: Expr, e1: Expr, e2: Expr) -> Result<i32, ()> {
        let c = self.eval(c)?;
        if c != 0 {
            self.eval(e1)
        } else {
            self.eval(e2)
        }
    }
}