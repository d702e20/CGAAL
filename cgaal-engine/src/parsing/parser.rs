use crate::parsing::ast::{
    BinaryOpKind, Coalition, CoalitionKind, Decl, DeclKind, Expr, ExprKind, Ident, LcgsRoot,
    PlayerDecl, RangeClause, RelabelCase, StateVarDecl, UnaryOpKind,
};
use crate::parsing::errors::ErrorLog;
use crate::parsing::lexer::Lexer;
use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};
use std::iter::Peekable;
use std::sync::Arc;

/// A helper macro for error recovery.
/// It tries to parse the given expression and then the given recovery token.
/// If it fails to parse expression, it tries to recover by skipping tokens until it finds any recovery token.
/// If the found recovery token is the expected one, it recovers and returns the error value.
/// If the found recovery token is not the expected one, it returns [RecoverMode] as an error such that the caller can try to recover.
/// This macro cannot be a function due to the borrow checker and the first argument self be a [Parser].
macro_rules! recover {
    ($self:expr, $val:expr, $recover_token:expr, $err_val:expr) => {{
        $self.recovery_tokens.push($recover_token);
        let res = match $val {
            Ok(v) => {
                match $self.lexer.peek() {
                    Some(Token { kind, .. }) if kind == &$recover_token => {
                        // Happy path - we parsed the expression and found the recovery token
                        let tok = $self.lexer.next().unwrap();
                        $self.recovery_tokens.pop();
                        Ok((tok.span, v))
                    }
                    Some(Token { kind, span }) => {
                        // The expression was parsed, but the recovery token was not found
                        $self.errors.log(
                            *span,
                            format!("Unexpected '{}', expected '{}'", kind, $recover_token),
                        );
                        Err(RecoverMode)
                    }
                    _ => {
                        $self
                            .errors
                            .log_msg(format!("Unexpected EOF, expected '{}'", $recover_token));
                        Err(RecoverMode)
                    }
                }
            }
            Err(_) => Err(RecoverMode),
        };
        res.or_else(|_| {
            // Unhappy path - Something went wrong
            // Try to recover by skipping tokens until we find a recovery token
            $self.skip_until_recovery();
            $self.recovery_tokens.pop();
            match $self.lexer.peek() {
                Some(Token { kind, .. }) if kind == &$recover_token => {
                    // Success recovery
                    let tok = $self.lexer.next().unwrap();
                    Ok((tok.span, $err_val))
                }
                _ => Err(RecoverMode),
            }
        })
    }};
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: &'a ErrorLog,
    /// A stack of tokens that can be used for error recovery.
    recovery_tokens: Vec<TokenKind>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, errors: &'a ErrorLog) -> Parser<'a> {
        Parser {
            lexer: lexer.peekable(),
            errors,
            recovery_tokens: vec![],
        }
    }

    /// Parse EOF. If there are any tokens left, log an error.
    pub fn expect_end(&mut self) {
        match self.lexer.next() {
            None => {}
            Some(tok) => {
                self.errors
                    .log(tok.span, format!("Unexpected '{}', expected EOF", tok.kind));
            }
        }
        for _ in self.lexer.by_ref() {}
    }

    /// Parse an LCGS program.
    pub fn lcgs_root(&mut self) -> Result<LcgsRoot, RecoverMode> {
        let decls = self.decls(false)?;
        let mut span = Span::new(0, 0);
        if !decls.is_empty() {
            span.begin = decls[0].span.begin;
            span.end = decls[decls.len() - 1].span.end;
        }
        Ok(LcgsRoot::new(span, decls))
    }

    /// Parse a list of declarations, assuming either global or template scope.
    pub fn decls(&mut self, in_template: bool) -> Result<Vec<Decl>, RecoverMode> {
        let mut decls = vec![];
        loop {
            match (self.lexer.peek(), in_template) {
                // Const declaration
                (
                    Some(Token {
                        kind: TokenKind::KwConst,
                        ..
                    }),
                    _,
                ) => {
                    let (_, decl) =
                        recover!(self, self.const_decl(), TokenKind::Semi, Decl::new_error())?;
                    decls.push(decl);
                }
                // State label declaration
                (
                    Some(Token {
                        kind: TokenKind::KwLabel,
                        ..
                    }),
                    _,
                ) => {
                    let (_, decl) = recover!(
                        self,
                        self.state_label_decl(),
                        TokenKind::Semi,
                        Decl::new_error()
                    )?;
                    decls.push(decl);
                }
                // State variable declaration
                (
                    Some(Token {
                        kind: TokenKind::Word(_),
                        ..
                    }),
                    _,
                ) => {
                    let (_, decl) = recover!(
                        self,
                        self.state_var_decl(),
                        TokenKind::Semi,
                        Decl::new_error()
                    )?;
                    decls.push(decl);
                }
                // Player declaration. Not allowed in templates
                (
                    Some(Token {
                        kind: TokenKind::KwPlayer,
                        ..
                    }),
                    false,
                ) => {
                    let (_, decl) =
                        recover!(self, self.player_decl(), TokenKind::Semi, Decl::new_error())?;
                    decls.push(decl);
                }
                // Template declaration. Not allowed in templates
                (
                    Some(Token {
                        kind: TokenKind::KwTemplate,
                        ..
                    }),
                    false,
                ) => {
                    decls.push(self.template_decl()?);
                }
                // Action declaration. Only allowed in templates
                (
                    Some(Token {
                        kind: TokenKind::Lbracket,
                        ..
                    }),
                    true,
                ) => {
                    let (_, decl) =
                        recover!(self, self.action_decl(), TokenKind::Semi, Decl::new_error())?;
                    decls.push(decl);
                }
                // Done
                (
                    Some(Token {
                        kind: TokenKind::KwEndTemplate,
                        ..
                    }),
                    _,
                ) if in_template => {
                    return Ok(decls);
                }
                // Done
                (None, _) => {
                    return Ok(decls);
                }
                // Unexpected
                (Some(_), _) => {
                    self.errors.log(
                        self.lexer.peek().unwrap().span,
                        format!(
                            "Unexpected '{}', expected declaration",
                            self.lexer.peek().unwrap().kind,
                        ),
                    );
                    return Err(RecoverMode);
                }
            }
        }
    }

    /// Parse a constant declaration.
    pub fn const_decl(&mut self) -> Result<Decl, RecoverMode> {
        let start = self.token(TokenKind::KwConst)?;
        let ident = self.ident()?;
        let _ = self.token(TokenKind::Assign)?;
        let expr = self.expr()?;
        let span = start + expr.span;
        let const_decl = DeclKind::Const(Arc::new(expr));
        Ok(Decl::new(span, ident, const_decl))
    }

    /// Parse a state label declaration.
    pub fn state_label_decl(&mut self) -> Result<Decl, RecoverMode> {
        let start = self.token(TokenKind::KwLabel)?;
        let ident = self.ident()?;
        let _ = self.token(TokenKind::Assign)?;
        let expr = self.expr()?;
        let span = start + expr.span;
        let state_label_decl = DeclKind::StateLabel(Arc::new(expr));
        Ok(Decl::new(span, ident, state_label_decl))
    }

    fn range_clause(&mut self) -> Result<RangeClause, RecoverMode> {
        let start = self.token(TokenKind::Lbracket)?;
        let (end, (min, max)) = recover!(
            self,
            self.range_clause_inner(),
            TokenKind::Rbracket,
            (Expr::new_error(), Expr::new_error())
        )?;
        Ok(RangeClause::new(start + end, min.into(), max.into()))
    }

    fn range_clause_inner(&mut self) -> Result<(Expr, Expr), RecoverMode> {
        let min = self.expr()?;
        let _ = self.token(TokenKind::DotDot)?;
        let max = self.expr()?;
        Ok((min, max))
    }

    /// Parse a state label declaration.
    pub fn state_var_decl(&mut self) -> Result<Decl, RecoverMode> {
        let ident = self.ident()?;
        self.token(TokenKind::Colon)?;
        let range = self.range_clause()?;
        let _ = self.token(TokenKind::KwInit)?;
        let init = self.expr()?;
        let _ = self.token(TokenKind::Semi)?;
        let update_ident = self.ident()?;
        let _ = self.token(TokenKind::Prime)?;
        let _ = self.token(TokenKind::Assign)?;
        let update = self.expr()?;
        let span = ident.span + update.span;
        let state_var = DeclKind::StateVar(Arc::new(StateVarDecl::new(
            range,
            init.into(),
            update_ident,
            update.into(),
        )));
        Ok(Decl::new(span, ident, state_var))
    }

    /// Parse a player declaration.
    pub fn player_decl(&mut self) -> Result<Decl, RecoverMode> {
        let mut span = self.token(TokenKind::KwPlayer)?;
        let ident = self.ident()?;
        let _ = self.token(TokenKind::Assign)?;
        let template = self.ident()?;
        let cases = match self.lexer.peek() {
            Some(Token {
                kind: TokenKind::Lbracket,
                ..
            }) => {
                let (relabel_span, cases) = self.relabelling()?;
                span.end = relabel_span.end;
                cases
            }
            _ => {
                span.end = template.span.end;
                Vec::new()
            }
        };
        let player = DeclKind::Player(Arc::new(PlayerDecl::new(template, cases)));
        Ok(Decl::new(span, ident, player))
    }

    /// Parse a relabelling.
    pub fn relabelling(&mut self) -> Result<(Span, Vec<RelabelCase>), RecoverMode> {
        let start = self.token(TokenKind::Lbracket)?;
        let (end, cases) = recover!(
            self,
            self.relabelling_inner(),
            TokenKind::Rbracket,
            Vec::new()
        )?;
        Ok((start + end, cases))
    }

    /// Parse the inners of a relabelling.
    fn relabelling_inner(&mut self) -> Result<Vec<RelabelCase>, RecoverMode> {
        let mut cases = vec![];
        #[allow(clippy::while_let_loop)]
        loop {
            match self.lexer.peek() {
                Some(Token {
                    kind: TokenKind::Word(_),
                    ..
                }) => {
                    let ident = self.ident()?;
                    let _ = self.token(TokenKind::Assign)?;
                    let expr = self.expr()?;
                    cases.push(RelabelCase::new(ident.span + expr.span, ident, expr.into()));
                }
                _ => break,
            }
            match self.lexer.peek() {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    self.lexer.next().unwrap();
                }
                Some(Token {
                    kind: TokenKind::Rbracket,
                    ..
                }) => break,
                Some(tok) => {
                    self.errors.log(
                        tok.span,
                        format!(
                            "Unexpected '{}', expected '{}' or '{}'",
                            tok.kind,
                            TokenKind::Comma,
                            self.recovery_tokens.last().unwrap()
                        ),
                    );
                    return Err(RecoverMode);
                }
                None => {
                    self.errors.log_msg(format!(
                        "Unexpected EOF, expected '{}' or '{}'",
                        TokenKind::Comma,
                        self.recovery_tokens.last().unwrap()
                    ));
                    return Err(RecoverMode);
                }
            }
        }
        Ok(cases)
    }

    /// Parse a template declaration.
    pub fn template_decl(&mut self) -> Result<Decl, RecoverMode> {
        let start = self.token(TokenKind::KwTemplate)?;
        let (end, (ident, decls)) = recover!(
            self,
            self.template_inner(),
            TokenKind::KwEndTemplate,
            (Ident::new_error(), Vec::new())
        )?;
        Ok(Decl::new(start + end, ident, DeclKind::Template(decls)))
    }

    /// Parse the inners a template declaration.
    fn template_inner(&mut self) -> Result<(Ident, Vec<Decl>), RecoverMode> {
        let ident = self.ident()?;
        let decls = self.decls(true)?;
        Ok((ident, decls))
    }

    /// Parse an action declaration.
    pub fn action_decl(&mut self) -> Result<Decl, RecoverMode> {
        let start = self.token(TokenKind::Lbracket)?;
        let (_, ident) = recover!(self, self.ident(), TokenKind::Rbracket, Ident::new_error())?;
        let cond = self.expr()?;
        let span = start + cond.span;
        let action = DeclKind::Action(Arc::new(cond));
        Ok(Decl::new(span, ident, action))
    }

    /// Parse an expression.
    pub fn expr(&mut self) -> Result<Expr, RecoverMode> {
        // The sub-expressions of a ternary cannot be another ternary in order to avoid ambiguity.
        // E.g. "a ? b : c ? d : f" is ambiguous and disallowed.
        let cond = self.binary_expr(0)?;
        match self.lexer.peek() {
            Some(Token {
                kind: TokenKind::Question,
                ..
            }) => {
                self.lexer.next().unwrap();
                let (_, then) = recover!(
                    self,
                    self.binary_expr(0),
                    TokenKind::Colon,
                    Expr::new_error()
                )?;
                let els = self.binary_expr(0)?;
                let span = cond.span + els.span;
                let kind = ExprKind::TernaryIf(cond.into(), then.into(), els.into());
                Ok(Expr::new(span, kind))
            }
            _ => Ok(cond),
        }
    }

    /// Parse an expression.
    pub fn binary_expr(&mut self, min_prec: u8) -> Result<Expr, RecoverMode> {
        // Pratt parsing/precedence climbing: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
        let mut lhs = self.term()?;
        let span_start = lhs.span;
        loop {
            let Some(op): Option<BinaryOpKind> = self
                .lexer
                .peek()
                .and_then(|t| t.kind.clone().try_into().ok())
            else {
                return Ok(lhs);
            };
            if op.precedence() < min_prec {
                return Ok(lhs);
            }
            self.lexer.next();
            let new_prec = op.precedence() + if op.is_right_associative() { 0 } else { 1 };
            let rhs = self.binary_expr(new_prec)?;
            let span = span_start + rhs.span;
            let kind = ExprKind::Binary(op, lhs.into(), rhs.into());
            lhs = Expr::new(span, kind);
        }
    }

    /// Parse a path expression.
    /// Path expressions treat `X`, `F`, `G`, `U` as temporal operators instead of identifiers.
    pub fn path_expr(&mut self) -> Result<Expr, RecoverMode> {
        match self.lexer.peek().map(|t| &t.kind) {
            Some(TokenKind::Lparen) => {
                let begin = self.lexer.next().unwrap().span;
                let (_, lhs) = recover!(
                    self,
                    self.expr(),
                    TokenKind::Word("U".to_string()),
                    Expr::new_error()
                )?;
                let (end, rhs) = recover!(self, self.expr(), TokenKind::Rparen, Expr::new_error())?;
                Ok(Expr::new(
                    begin + end,
                    ExprKind::Binary(BinaryOpKind::Until, lhs.into(), rhs.into()),
                ))
            }
            Some(TokenKind::Word(w)) if w == "F" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr()?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Eventually, expr.into()),
                ))
            }
            Some(TokenKind::Word(w)) if w == "G" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr()?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Invariantly, expr.into()),
                ))
            }
            Some(TokenKind::Word(w)) if w == "X" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr()?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Next, expr.into()),
                ))
            }
            // Unexpected cases
            Some(_) => {
                let tok = self.lexer.next().unwrap();
                self.errors.log(
                    tok.span,
                    format!(
                        "Unexpected '{}', expected 'X', 'F', 'G', or '(... U ...)'",
                        tok.kind
                    ),
                );
                Err(RecoverMode)
            }
            None => {
                self.errors.log_msg(
                    "Unexpected EOF, expected 'X', 'F', 'G', or '(... U ...)'".to_string(),
                );
                Err(RecoverMode)
            }
        }
    }

    /// Parse an expression term.
    pub fn term(&mut self) -> Result<Expr, RecoverMode> {
        // TODO: min/max functions
        match self.lexer.peek().map(|t| &t.kind) {
            Some(TokenKind::Lparen) => self.paren(),
            Some(TokenKind::Bang) => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.term()?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Not, expr.into()),
                ))
            }
            Some(TokenKind::Minus) => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.term()?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Neg, expr.into()),
                ))
            }
            Some(TokenKind::KwMax | TokenKind::KwMin) => self.func_call(),
            Some(TokenKind::True) => {
                let tok = self.lexer.next().unwrap();
                Ok(Expr::new(tok.span, ExprKind::True))
            }
            Some(TokenKind::False) => {
                let tok = self.lexer.next().unwrap();
                Ok(Expr::new(tok.span, ExprKind::False))
            }
            Some(TokenKind::Num(_)) => {
                let tok = self.lexer.next().unwrap();
                Ok(Expr::new(tok.span, ExprKind::Num(tok.num().unwrap())))
            }
            Some(TokenKind::Word(_)) => self.owned_ident(),
            Some(TokenKind::Llangle) => self.enforce_coalition(),
            Some(TokenKind::Llbracket) => self.despite_coalition(),
            // Unexpected
            Some(_) => {
                let tok = self.lexer.peek().unwrap();
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected expression term", tok.kind),
                );
                Err(RecoverMode)
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected expression term".to_string());
                Err(RecoverMode)
            }
        }
    }

    /// Parse a parenthesized expression.
    pub fn paren(&mut self) -> Result<Expr, RecoverMode> {
        let begin = self.token(TokenKind::Lparen)?;
        recover!(self, self.expr(), TokenKind::Rparen, Expr::new_error())
            .map(|(end, expr)| Expr::new(begin + end, ExprKind::Paren(Arc::new(expr))))
    }

    /// Parse a function call
    pub fn func_call(&mut self) -> Result<Expr, RecoverMode> {
        match self.lexer.peek() {
            Some(Token {
                kind: TokenKind::KwMax,
                ..
            }) => {
                let begin = self.lexer.next().unwrap().span;
                let _ = self.token(TokenKind::Lparen)?;
                let (end, exprs) = recover!(self, self.args(1), TokenKind::Rparen, Vec::new())?;
                Ok(Expr::new(begin + end, ExprKind::Max(exprs)))
            }
            Some(Token {
                kind: TokenKind::KwMin,
                ..
            }) => {
                let begin = self.lexer.next().unwrap().span;
                let _ = self.token(TokenKind::Lparen)?;
                let (end, exprs) = recover!(self, self.args(1), TokenKind::Rparen, Vec::new())?;
                Ok(Expr::new(begin + end, ExprKind::Min(exprs)))
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected 'max' or 'min'".to_string());
                Err(RecoverMode)
            }
            Some(Token { kind, span }) => {
                self.errors.log(
                    *span,
                    format!("Unexpected '{}', expected 'max' or 'min'", kind),
                );
                Err(RecoverMode)
            }
        }
    }

    /// Parse an (owned) identifier, e.g. `p1` or `p1.attr`.
    pub fn owned_ident(&mut self) -> Result<Expr, RecoverMode> {
        let lhs = self.ident()?;
        if !matches!(
            self.lexer.peek(),
            Some(Token {
                kind: TokenKind::Dot,
                ..
            })
        ) {
            return Ok(Expr::new(lhs.span, ExprKind::OwnedIdent(None, lhs)));
        }
        let _ = self.token(TokenKind::Dot)?;
        let rhs = self.ident()?;
        Ok(Expr::new(
            lhs.span + rhs.span,
            ExprKind::OwnedIdent(Some(lhs), rhs),
        ))
    }

    /// Parse an identifier.
    pub fn ident(&mut self) -> Result<Ident, RecoverMode> {
        match self.lexer.peek() {
            Some(Token {
                kind: TokenKind::Word(_),
                ..
            }) => {
                let tok = self.lexer.next().unwrap();
                Ok(Ident::new(tok.span, tok.kind.to_string()))
            }
            Some(tok) => {
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected identifier", tok.kind),
                );
                Err(RecoverMode)
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected identifier".to_string());
                Err(RecoverMode)
            }
        }
    }

    /// Parse a comma-separated list of expressions.
    /// If `min_count` is given, it will try to parse at least that many expressions.
    fn args(&mut self, min_count: usize) -> Result<Vec<Expr>, RecoverMode> {
        let mut exprs = Vec::new();
        #[allow(clippy::while_let_loop)]
        loop {
            match self.lexer.peek() {
                Some(Token {
                    // Expr first set
                    kind:
                        TokenKind::Lparen
                        | TokenKind::Bang
                        | TokenKind::Minus
                        | TokenKind::True
                        | TokenKind::False
                        | TokenKind::Num(_)
                        | TokenKind::Word(_)
                        | TokenKind::Llangle
                        | TokenKind::Llbracket,
                    ..
                }) => {
                    let expr = self.expr()?;
                    exprs.push(expr);
                }
                _ => break,
            }
            match self.lexer.peek() {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    self.lexer.next().unwrap();
                }
                Some(Token {
                    kind: TokenKind::Rparen,
                    ..
                }) => break,
                Some(tok) => {
                    self.errors.log(
                        tok.span,
                        format!(
                            "Unexpected '{}', expected '{}' or '{}'",
                            tok.kind,
                            TokenKind::Comma,
                            self.recovery_tokens.last().unwrap()
                        ),
                    );
                    return Err(RecoverMode);
                }
                None => {
                    self.errors.log_msg(format!(
                        "Unexpected EOF, expected '{}' or '{}'",
                        TokenKind::Comma,
                        self.recovery_tokens.last().unwrap()
                    ));
                    return Err(RecoverMode);
                }
            }
        }
        if exprs.len() < min_count {
            self.errors.log_msg(format!(
                "Expected at least {} argument(s), got {}",
                min_count,
                exprs.len()
            ));
            return Err(RecoverMode);
        }
        Ok(exprs)
    }

    /// Parse an enforce-coalition expression.
    pub fn enforce_coalition(&mut self) -> Result<Expr, RecoverMode> {
        let begin = self.token(TokenKind::Llangle)?;
        let (end, players) = recover!(self, self.coalition_players(), TokenKind::Rrangle, vec![])?;
        let expr = self.path_expr()?;
        Ok(Expr::new(
            begin + expr.span,
            ExprKind::Coalition(Coalition::new(
                begin + end,
                players,
                CoalitionKind::Enforce,
                Arc::new(expr),
            )),
        ))
    }

    /// Parse a despite-coalition expression.
    pub fn despite_coalition(&mut self) -> Result<Expr, RecoverMode> {
        let begin = self.token(TokenKind::Llbracket)?;
        let (end, players) =
            recover!(self, self.coalition_players(), TokenKind::Rrbracket, vec![])?;
        let expr = self.path_expr()?;
        Ok(Expr::new(
            begin + expr.span,
            ExprKind::Coalition(Coalition::new(
                begin + end,
                players,
                CoalitionKind::Despite,
                Arc::new(expr),
            )),
        ))
    }

    /// Parse a comma-separated list of players (with the assumption that we are inside a coalition.)
    pub fn coalition_players(&mut self) -> Result<Vec<Ident>, RecoverMode> {
        let mut players = vec![];
        #[allow(clippy::while_let_loop)]
        loop {
            match self.lexer.peek() {
                Some(Token {
                    kind: TokenKind::Word(_),
                    ..
                }) => {
                    let ident = self.ident()?;
                    players.push(ident);
                }
                _ => break,
            }
            match self.lexer.peek() {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    self.lexer.next().unwrap();
                }
                Some(Token {
                    kind: TokenKind::Rrangle | TokenKind::Rrbracket,
                    ..
                }) => break,
                Some(tok) => {
                    self.errors.log(
                        tok.span,
                        format!(
                            "Unexpected '{}', expected '{}' or '{}'",
                            tok.kind,
                            TokenKind::Comma,
                            self.recovery_tokens.last().unwrap()
                        ),
                    );
                    return Err(RecoverMode);
                }
                _ => {
                    self.errors.log_msg(format!(
                        "Unexpected EOF, expected '{}' or '{}'",
                        TokenKind::Comma,
                        self.recovery_tokens.last().unwrap()
                    ));
                    return Err(RecoverMode);
                }
            }
        }
        Ok(players)
    }

    /// Parse a token of the given kind.
    fn token(&mut self, kind: TokenKind) -> Result<Span, RecoverMode> {
        match self.lexer.next() {
            Some(tok) if tok.kind == kind => Ok(tok.span),
            Some(tok) => {
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected '{}'", tok.kind, kind),
                );
                Err(RecoverMode)
            }
            None => {
                self.errors
                    .log_msg(format!("Unexpected EOF, expected '{}'", kind));
                Err(RecoverMode)
            }
        }
    }

    /// Skip tokens until the next is a recovery token.
    fn skip_until_recovery(&mut self) {
        while let Some(tok) = self.lexer.peek() {
            if self.recovery_tokens.contains(&tok.kind) {
                return;
            }
            self.lexer.next();
        }
    }
}

/// A marker type for error recovery. If a function returns this, it means that it failed to
/// parse the input and the caller should try to recover.
#[derive(Debug, Eq, PartialEq)]
pub struct RecoverMode;
