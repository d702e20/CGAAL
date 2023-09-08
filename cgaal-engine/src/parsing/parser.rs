use crate::parsing::ast::{BinaryOpKind, Coalition, CoalitionKind, Expr, ExprKind, UnaryOpKind};
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
/// This macro cannot be a function due to the borrow checker.
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
    errors: &'a mut ErrorLog,
    /// A stack of tokens that can be used for error recovery.
    recovery_tokens: Vec<TokenKind>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, errors: &'a mut ErrorLog) -> Parser<'a> {
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

    /// Parse an expression.
    pub fn expr(&mut self, min_prec: u8) -> Result<Expr, RecoverMode> {
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
            let rhs = self.expr(new_prec)?;
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
                    self.expr(BinaryOpKind::Until.precedence()),
                    TokenKind::Word("U".to_string()),
                    Expr::new_error()
                )?;
                let (end, rhs) =
                    recover!(self, self.expr(0), TokenKind::Rparen, Expr::new_error())?;
                Ok(Expr::new(
                    begin + end,
                    ExprKind::Binary(BinaryOpKind::Until, lhs.into(), rhs.into()),
                ))
            }
            Some(TokenKind::Word(w)) if w == "F" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0)?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Eventually, expr.into()),
                ))
            }
            Some(TokenKind::Word(w)) if w == "G" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0)?;
                Ok(Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Invariantly, expr.into()),
                ))
            }
            Some(TokenKind::Word(w)) if w == "X" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0)?;
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
                    format!("Unexpected '{}', expected path expression", tok.kind),
                );
                Err(RecoverMode)
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected path expression".to_string());
                Err(RecoverMode)
            }
        }
    }

    /// Parse an expression term.
    pub fn term(&mut self) -> Result<Expr, RecoverMode> {
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
            Some(TokenKind::True) => {
                let tok = self.lexer.next().unwrap();
                Ok(Expr::new(tok.span, ExprKind::True))
            }
            Some(TokenKind::False) => {
                let tok = self.lexer.next().unwrap();
                Ok(Expr::new(tok.span, ExprKind::False))
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
        recover!(self, self.expr(0), TokenKind::Rparen, Expr::new_error())
            .map(|(end, expr)| Expr::new(begin + end, ExprKind::Paren(Arc::new(expr))))
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
            return Ok(lhs);
        }
        let _ = self.token(TokenKind::Dot)?;
        let rhs = self.ident()?;
        Ok(Expr::new(
            lhs.span + rhs.span,
            ExprKind::Binary(BinaryOpKind::Dot, lhs.into(), rhs.into()),
        ))
    }

    /// Parse an identifier.
    pub fn ident(&mut self) -> Result<Expr, RecoverMode> {
        match self.lexer.peek() {
            Some(Token {
                kind: TokenKind::Word(_),
                ..
            }) => {
                let tok = self.lexer.next().unwrap();
                Ok(Expr::new(tok.span, ExprKind::Ident(tok.kind.to_string())))
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
    pub fn coalition_players(&mut self) -> Result<Vec<Expr>, RecoverMode> {
        let mut players = vec![];
        #[allow(clippy::while_let_loop)]
        loop {
            match self.lexer.peek() {
                Some(Token {
                    kind: TokenKind::Word(_),
                    ..
                }) => {
                    let tok = self.lexer.next().unwrap();
                    let p = Expr::new(tok.span, ExprKind::Ident(tok.kind.to_string()));
                    players.push(p);
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
