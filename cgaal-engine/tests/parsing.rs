use cgaal_engine::parsing::ast::*;
use cgaal_engine::parsing::lexer::*;
use cgaal_engine::parsing::parser::*;
use cgaal_engine::parsing::span::*;

#[test]
fn basic_expr_001() {
    let input = "true && false";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(
        parser.errors.is_empty(),
        "ErrorLog is not empty: {:?}",
        parser.errors
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 13),
            ExprKind::Binary(
                BinaryOpKind::And,
                Expr::new(Span::new(0, 4), ExprKind::True).into(),
                Expr::new(Span::new(8, 13), ExprKind::False).into()
            )
        )
    );
}

#[test]
fn basic_expr_002() {
    let input = "true && false || true";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(
        parser.errors.is_empty(),
        "ErrorLog is not empty: {:?}",
        parser.errors
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 21),
            ExprKind::Binary(
                BinaryOpKind::Or,
                Expr::new(
                    Span::new(0, 13),
                    ExprKind::Binary(
                        BinaryOpKind::And,
                        Expr::new(Span::new(0, 4), ExprKind::True).into(),
                        Expr::new(Span::new(8, 13), ExprKind::False).into()
                    )
                )
                .into(),
                Expr::new(Span::new(17, 21), ExprKind::True).into()
            )
        )
    );
}

#[test]
fn basic_expr_003() {
    let input = "foo || true && bar || (true || false)";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(
        parser.errors.is_empty(),
        "ErrorLog is not empty: {:?}",
        parser.errors
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 37),
            ExprKind::Binary(
                BinaryOpKind::Or,
                Expr::new(
                    Span::new(0, 18),
                    ExprKind::Binary(
                        BinaryOpKind::Or,
                        Expr::new(Span::new(0, 3), ExprKind::Ident("foo".to_string())).into(),
                        Expr::new(
                            Span::new(7, 18),
                            ExprKind::Binary(
                                BinaryOpKind::And,
                                Expr::new(Span::new(7, 11), ExprKind::True).into(),
                                Expr::new(Span::new(15, 18), ExprKind::Ident("bar".to_string()))
                                    .into()
                            )
                        )
                        .into()
                    )
                )
                .into(),
                Expr::new(
                    Span::new(22, 37),
                    ExprKind::Paren(
                        Expr::new(
                            Span::new(23, 36),
                            ExprKind::Binary(
                                BinaryOpKind::Or,
                                Expr::new(Span::new(23, 27), ExprKind::True).into(),
                                Expr::new(Span::new(31, 36), ExprKind::False).into()
                            )
                        )
                        .into()
                    )
                )
                .into()
            )
            .into()
        )
    );
}

#[test]
fn atl_expr_001() {
    let input = "<<p1>> F goal";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(
        parser.errors.is_empty(),
        "ErrorLog is not empty: {:?}",
        parser.errors
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 13),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 6),
                vec![Expr::new(
                    Span::new(2, 4),
                    ExprKind::Ident("p1".to_string())
                )],
                CoalitionKind::Enforce,
                Expr::new(
                    Span::new(7, 13),
                    ExprKind::Unary(
                        UnaryOpKind::Eventually,
                        Expr::new(Span::new(9, 13), ExprKind::Ident("goal".to_string())).into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn atl_expr_002() {
    let input = "[[p1, p2]] G safe";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(
        parser.errors.is_empty(),
        "ErrorLog is not empty: {:?}",
        parser.errors
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 17),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 10),
                vec![
                    Expr::new(Span::new(2, 4), ExprKind::Ident("p1".to_string())),
                    Expr::new(Span::new(6, 8), ExprKind::Ident("p2".to_string())),
                ],
                CoalitionKind::Despite,
                Expr::new(
                    Span::new(11, 17),
                    ExprKind::Unary(
                        UnaryOpKind::Invariantly,
                        Expr::new(Span::new(13, 17), ExprKind::Ident("safe".to_string())).into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn atl_expr_003() {
    let input = "<<>> (safe U goal)";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(
        parser.errors.is_empty(),
        "ErrorLog is not empty: {:?}",
        parser.errors
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 18),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 4),
                vec![],
                CoalitionKind::Enforce,
                Expr::new(
                    Span::new(5, 18),
                    ExprKind::Binary(
                        BinaryOpKind::Until,
                        Expr::new(Span::new(6, 10), ExprKind::Ident("safe".to_string())).into(),
                        Expr::new(Span::new(13, 17), ExprKind::Ident("goal".to_string())).into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn erroneous_expr_001() {
    let input = "true && ";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(parser.errors.has_errors());
    let mut out = String::new();
    parser.errors.write_detailed(input, &mut out).unwrap();
    assert_eq!(
        out,
        "\x1b[93m@ Error:\x1b[0m Unexpected EOF, expected expression term\n"
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 4),
            ExprKind::Binary(
                BinaryOpKind::And,
                Expr::new(Span::new(0, 4), ExprKind::True).into(),
                Expr::new_error().into()
            )
        )
    );
}

#[test]
fn erroneous_expr_002() {
    let input = "foo bar";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(parser.errors.has_errors());
    let mut out = String::new();
    parser.errors.write_detailed(input, &mut out).unwrap();
    assert_eq!(
        out,
        "\x1b[93m1:5 Error:\x1b[0m Unexpected 'bar', expected EOF\n\
        | foo bar\n\
        |     ^^^\n"
    );
    assert_eq!(
        expr,
        Expr::new(Span::new(0, 3), ExprKind::Ident("foo".to_string()))
    );
}

#[test]
fn erroneous_expr_003() {
    let input = "(foo false) && true";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(parser.errors.has_errors());
    let mut out = String::new();
    parser.errors.write_detailed(input, &mut out).unwrap();
    assert_eq!(
        out,
        "\x1b[93m1:6 Error:\x1b[0m Unexpected 'false', expected ')'\n\
        | (foo false) && true\n\
        |      ^^^^^\n"
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 19),
            ExprKind::Binary(
                BinaryOpKind::And,
                Expr::new(
                    Span::new(0, 11),
                    ExprKind::Paren(
                        Expr::new(Span::new(1, 4), ExprKind::Ident("foo".to_string()),).into()
                    )
                )
                .into(),
                Expr::new(Span::new(15, 19), ExprKind::True).into()
            )
        )
    );
}

#[test]
fn erroneous_expr_004() {
    let input = "(true";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(parser.errors.has_errors());
    let mut out = String::new();
    parser.errors.write_detailed(input, &mut out).unwrap();
    assert_eq!(
        out,
        "\x1b[93m@ Error:\x1b[0m Unexpected EOF, expected ')'\n"
    );
    assert_eq!(expr, Expr::new_error());
}

#[test]
fn erroneous_expr_005() {
    let input = "(foo bar";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(parser.errors.has_errors());
    let mut out = String::new();
    parser.errors.write_detailed(input, &mut out).unwrap();
    assert_eq!(
        out,
        "\x1b[93m1:6 Error:\x1b[0m Unexpected 'bar', expected ')'\n\
        | (foo bar\n\
        |      ^^^\n"
    );
    assert_eq!(expr, Expr::new_error());
}

#[test]
fn erroneous_expr_006() {
    let input = " ( << p1 err goal)";
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let expr = parser.expr(0);
    parser.expect_end();
    assert!(parser.errors.has_errors());
    let mut out = String::new();
    parser.errors.write_detailed(input, &mut out).unwrap();
    assert_eq!(
        out,
        "\x1b[93m1:10 Error:\x1b[0m Unexpected 'err', expected '>>'\n\
        |  ( << p1 err goal)\n\
        |          ^^^\n"
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(1, 19),
            ExprKind::Paren(Expr::new_error().into())
        )
    );
}
