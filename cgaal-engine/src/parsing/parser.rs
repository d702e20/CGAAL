use crate::parsing::ast::{BinaryOpKind, Coalition, CoalitionKind, Expr, ExprKind, UnaryOpKind};
use crate::parsing::errors::ErrorLog;
use crate::parsing::lexer::Lexer;
use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};
use std::iter::Peekable;
use std::sync::Arc;

macro_rules! recover {
    ($self:expr, $val:expr, $recover_token:expr, $err_val:expr) => {{
        $self.recovery_tokens.push($recover_token);
        let res = match $val {
            Ok(v) => {
                match $self.lexer.peek() {
                    Some(Token { kind, .. }) if kind == &$recover_token => {
                        // Happy path
                        let tok = $self.lexer.next().unwrap();
                        $self.recovery_tokens.pop();
                        Ok((tok.span, v))
                    }
                    Some(Token { kind, span }) => {
                        $self.errors.log(
                            *span,
                            format!("Unexpected '{}', expected '{}'", kind, $recover_token),
                        );
                        Err(ParseError)
                    }
                    _ => {
                        $self
                            .errors
                            .log_msg(format!("Unexpected EOF, expected '{}'", $recover_token));
                        Err(ParseError)
                    }
                }
            }
            Err(_) => Err(ParseError),
        };
        res.or_else(|_| {
            // Unhappy path
            // Try recover
            $self.skip_until_recovery();
            $self.recovery_tokens.pop();
            match $self.lexer.peek() {
                Some(Token { kind, .. }) if kind == &$recover_token => {
                    let tok = $self.lexer.next().unwrap();
                    Ok((tok.span, $err_val))
                }
                _ => Err(ParseError),
            }
        })
    }};
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: &'a mut ErrorLog,
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

    pub fn expect_end(&mut self) {
        match self.lexer.next() {
            None => {}
            Some(tok) => {
                self.errors
                    .log(tok.span, format!("Unexpected '{}', expected EOF", tok.kind));
            }
        }
        for _ in self.lexer.by_ref() {};
    }

    pub fn expr(&mut self, min_prec: u8) -> Result<Expr, ParseError> {
        // Pratt parsing/precedence climbing: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
        let mut lhs = self.term()?;
        let span_start = lhs.span;
        loop {
            let Some(op): Option<BinaryOpKind> = self.lexer.peek().and_then(|t| t.kind().clone().try_into().ok()) else { return Ok(lhs) };
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

    pub fn path_expr(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek().map(|t| t.kind()) {
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
            // Unexpected
            Some(_) => {
                let tok = self.lexer.next().unwrap();
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected path expression", tok.kind),
                );
                Err(ParseError)
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected path expression".to_string());
                Err(ParseError)
            }
        }
    }

    pub fn term(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek().map(|t| t.kind()) {
            Some(TokenKind::Lparen) => self.paren(),
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
                Err(ParseError)
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected expression term".to_string());
                Err(ParseError)
            }
        }
    }

    pub fn paren(&mut self) -> Result<Expr, ParseError> {
        let begin = self.token(TokenKind::Lparen)?;
        recover!(self, self.expr(0), TokenKind::Rparen, Expr::new_error())
            .map(|(end, expr)| Expr::new(begin + end, ExprKind::Paren(Arc::new(expr))))
    }

    pub fn owned_ident(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.ident()?;
        if !matches!(self.lexer.peek(), Some(Token { kind: TokenKind::Dot, .. })) {
            return Ok(lhs);
        }
        let _ = self.token(TokenKind::Dot)?;
        let rhs = self.ident()?;
        Ok(Expr::new(
            lhs.span + rhs.span,
            ExprKind::Binary(BinaryOpKind::Dot, lhs.into(), rhs.into()),
        ))
    }

    pub fn ident(&mut self) -> Result<Expr, ParseError> {
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
                Err(ParseError)
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected identifier".to_string());
                Err(ParseError)
            }
        }
    }

    pub fn enforce_coalition(&mut self) -> Result<Expr, ParseError> {
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

    pub fn despite_coalition(&mut self) -> Result<Expr, ParseError> {
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

    pub fn coalition_players(&mut self) -> Result<Vec<Expr>, ParseError> {
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
                    return Err(ParseError);
                }
                _ => {
                    self.errors.log_msg(format!(
                        "Unexpected EOF, expected '{}' or '{}'",
                        TokenKind::Comma,
                        self.recovery_tokens.last().unwrap()
                    ));
                    return Err(ParseError);
                }
            }
        }
        Ok(players)
    }

    fn token(&mut self, kind: TokenKind) -> Result<Span, ParseError> {
        match self.lexer.next() {
            Some(tok) if tok.kind == kind => Ok(tok.span),
            Some(tok) => {
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected '{}'", tok.kind, kind),
                );
                Err(ParseError)
            }
            None => {
                self.errors
                    .log_msg(format!("Unexpected EOF, expected '{}'", kind));
                Err(ParseError)
            }
        }
    }

    fn skip_until_recovery(&mut self) {
        while let Some(tok) = self.lexer.peek() {
            if self.recovery_tokens.contains(&tok.kind) {
                return;
            }
            self.lexer.next();
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError;
