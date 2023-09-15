use cgaal_engine::atl::convert::convert_expr_to_phi;
use cgaal_engine::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
use cgaal_engine::game_structure::lcgs::parse::parse_lcgs;
use cgaal_engine::parsing::ast::*;
use cgaal_engine::parsing::errors::ErrorLog;
use cgaal_engine::parsing::lexer::*;
use cgaal_engine::parsing::parse_atl;
use cgaal_engine::parsing::parser::*;
use cgaal_engine::parsing::span::*;

#[test]
fn basic_expr_001() {
    // Check true and false
    let input = "true && false";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to valid parse expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
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
    // Check precedence of && over ||
    let input = "true && false || true";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to valid parse expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
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
    // Check precedence of && over || and parenthesis
    let input = "foo || true && bar.baz || (true || false)";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to valid parse expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 41),
            ExprKind::Binary(
                BinaryOpKind::Or,
                Expr::new(
                    Span::new(0, 22),
                    ExprKind::Binary(
                        BinaryOpKind::Or,
                        Expr::new(
                            Span::new(0, 3),
                            ExprKind::OwnedIdent(
                                None,
                                Ident::new(Span::new(0, 3), "foo".to_string())
                            )
                        )
                        .into(),
                        Expr::new(
                            Span::new(7, 22),
                            ExprKind::Binary(
                                BinaryOpKind::And,
                                Expr::new(Span::new(7, 11), ExprKind::True).into(),
                                Expr::new(
                                    Span::new(15, 22),
                                    ExprKind::OwnedIdent(
                                        Some(Ident::new(Span::new(15, 18), "bar".to_string())),
                                        Ident::new(Span::new(19, 22), "baz".to_string())
                                    )
                                )
                                .into()
                            )
                        )
                        .into()
                    )
                )
                .into(),
                Expr::new(
                    Span::new(26, 41),
                    ExprKind::Paren(
                        Expr::new(
                            Span::new(27, 40),
                            ExprKind::Binary(
                                BinaryOpKind::Or,
                                Expr::new(Span::new(27, 31), ExprKind::True).into(),
                                Expr::new(Span::new(35, 40), ExprKind::False).into()
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
    // Check single player in coalition and eventually operator
    let input = "<<p1>> F goal";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to valid parse expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 13),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 6),
                vec![Ident::new(Span::new(2, 4), "p1".to_string()).into()],
                CoalitionKind::Enforce,
                Expr::new(
                    Span::new(7, 13),
                    ExprKind::Unary(
                        UnaryOpKind::Eventually,
                        Expr::new(
                            Span::new(9, 13),
                            ExprKind::OwnedIdent(
                                None,
                                Ident::new(Span::new(9, 13), "goal".to_string())
                            )
                        )
                        .into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn atl_expr_002() {
    // Check multiple players in coalition and invariant operator
    let input = "[[p1, p2]] G safe";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to valid parse expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 17),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 10),
                vec![
                    Ident::new(Span::new(2, 4), "p1".to_string()),
                    Ident::new(Span::new(6, 8), "p2".to_string()),
                ],
                CoalitionKind::Despite,
                Expr::new(
                    Span::new(11, 17),
                    ExprKind::Unary(
                        UnaryOpKind::Invariantly,
                        Expr::new(
                            Span::new(13, 17),
                            ExprKind::OwnedIdent(
                                None,
                                Ident::new(Span::new(13, 17), "safe".to_string())
                            )
                        )
                        .into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn atl_expr_003() {
    // Check empty coalition and until operator
    let input = "<<>> (safe U goal)";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Error should be recoverable");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
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
                        Expr::new(
                            Span::new(6, 10),
                            ExprKind::OwnedIdent(
                                None,
                                Ident::new(Span::new(6, 10), "safe".to_string())
                            )
                        )
                        .into(),
                        Expr::new(
                            Span::new(13, 17),
                            ExprKind::OwnedIdent(
                                None,
                                Ident::new(Span::new(13, 17), "goal".to_string())
                            )
                        )
                        .into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn atl_expr_004() {
    // Check attribute access
    let input = "<<p1>> F p1.attr";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to valid parse expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 16),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 6),
                vec![Ident::new(Span::new(2, 4), "p1".to_string(),)],
                CoalitionKind::Enforce,
                Expr::new(
                    Span::new(7, 16),
                    ExprKind::Unary(
                        UnaryOpKind::Eventually,
                        Expr::new(
                            Span::new(9, 16),
                            ExprKind::OwnedIdent(
                                Some(Ident::new(Span::new(9, 11), "p1".to_string())),
                                Ident::new(Span::new(12, 16), "attr".to_string()),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            )),
        )
    );
}

#[test]
fn atl_expr_005() {
    // Is comma at the end of coalition list accepted?
    let input = "<<p1,>> F safe";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Failed to parse valid expression");
    parser.expect_end();
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 14),
            ExprKind::Coalition(Coalition::new(
                Span::new(0, 7),
                vec![Ident::new(Span::new(2, 4), "p1".to_string())],
                CoalitionKind::Enforce,
                Expr::new(
                    Span::new(8, 14),
                    ExprKind::Unary(
                        UnaryOpKind::Eventually,
                        Expr::new(
                            Span::new(10, 14),
                            ExprKind::OwnedIdent(
                                None,
                                Ident::new(Span::new(10, 14), "safe".to_string())
                            )
                        )
                        .into()
                    )
                )
                .into()
            ))
        )
    );
}

#[test]
fn erroneous_expr_001() {
    // Check if unexpected EOF is reported correctly
    let input = "true && ";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    assert!(parser.expr(0).is_err());
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\x1b[31m@ Error:\x1b[0m Unexpected EOF, expected expression term\n"
    );
}

#[test]
fn erroneous_expr_002() {
    // Check if error report when EOF is expected
    let input = "foo bar";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Error should be recoverable");
    parser.expect_end();
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\x1b[31m1:5 Error:\x1b[0m Unexpected 'bar', expected EOF\n\
        | foo bar\n\
        |     ^^^\n"
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 3),
            ExprKind::OwnedIdent(None, Ident::new(Span::new(0, 3), "foo".to_string()))
        )
    );
}

#[test]
fn erroneous_expr_003() {
    // Check if error recovery works on parenthesis
    let input = "(foo false) && true";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Error should be recoverable");
    parser.expect_end();
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\x1b[31m1:6 Error:\x1b[0m Unexpected 'false', expected ')'\n\
        | (foo false) && true\n\
        |      ^^^^^\n"
    );
    assert_eq!(
        expr,
        Expr::new(
            Span::new(0, 19),
            ExprKind::Binary(
                BinaryOpKind::And,
                Expr::new(Span::new(0, 11), ExprKind::Paren(Expr::new_error().into(),)).into(),
                Expr::new(Span::new(15, 19), ExprKind::True).into()
            )
        )
    );
}

#[test]
fn erroneous_expr_004() {
    // Check if unrecoverable error is detected
    let input = "(true";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    assert!(parser.expr(0).is_err());
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\x1b[31m@ Error:\x1b[0m Unexpected EOF, expected ')'\n"
    );
}

#[test]
fn erroneous_expr_005() {
    // Check error inside unclosed parenthesis is reported correctly
    let input = "(foo bar";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    assert!(parser.expr(0).is_err());
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\x1b[31m1:6 Error:\x1b[0m Unexpected 'bar', expected ')'\n\
        | (foo bar\n\
        |      ^^^\n"
    );
}

#[test]
fn erroneous_expr_006() {
    // Check error missing >> inside parenthesis is reported correctly
    let input = " ( << p1 foo goal)";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let expr = parser.expr(0).expect("Error should be recoverable");
    parser.expect_end();
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\x1b[31m1:10 Error:\x1b[0m Unexpected 'foo', expected ',' or '>>'\n\
        |  ( << p1 foo goal)\n\
        |          ^^^\n"
    );
    assert_eq!(
        expr,
        Expr::new(Span::new(1, 18), ExprKind::Paren(Expr::new_error().into()))
    );
}

#[test]
fn erroneous_expr_007() {
    // Check error on subsequent attribute access
    let input = "<<p1>> G p1.attr1.attr2";
    let mut errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, &mut errors);
    let _expr = parser.expr(0).expect("Error should be recoverable");
    parser.expect_end();
    assert!(errors.has_errors());
    let out = errors.to_string(input);
    assert_eq!(
        out,
        "\u{1b}[31m1:18 Error:\u{1b}[0m Unexpected '.', expected EOF\n\
        | <<p1>> G p1.attr1.attr2\n\
        |                  ^\n"
    );
}

#[test]
fn atl_expr_batch() {
    // Check that no errors are found for valid ATL expressions
    let lcgs_raw = "\n\
    player p1 = thing;\n\
    player p2 = thing;\n\
    \n\
    label prop = 1;\n\
    \n\
    template thing\n\
        x : [0..10] init 0;\n\
        x' = x;\n\
        \n\
        label attr = 1;\n\
        \n\
        [wait] 1;\n\
    endtemplate\n";

    let atls = [
        "true",
        "false",
        "prop",
        "(prop && prop || !prop) && !prop || prop || prop && !!prop",
        "p1.attr && p2.attr",
        "(prop && (prop && (prop) || !!!((prop))))",
        "<<>> F p1.attr",
        "<<p1>> G p1.attr",
        "<<p1, p2>> X p2.attr",
        "[[]] (prop U p1.attr)",
        "[[p1, p2]] (prop U p2.attr)",
        "prop && <<p1>> F p1.attr && !prop",
        "[[p1]] F false || [[p2]] G true",
        "[[p2]] (<<p1>> X p1.attr && !prop U true || false)",
    ];

    let root = parse_lcgs(lcgs_raw).unwrap();
    let game = IntermediateLcgs::create(root).unwrap();

    for atl_raw in atls {
        let mut errors = ErrorLog::new();
        parse_atl(atl_raw, &mut errors)
            .and_then(|expr| convert_expr_to_phi(&expr, &game, &mut errors))
            .expect(&format!(
                "For '{}', ErrorLog is not empty: {:?}",
                atl_raw, errors
            ));
    }
}

#[test]
fn atl_expr_error_batch() {
    // Check that errors are found for erroneous ATL expressions
    let lcgs_raw = "\n\
    player p1 = thing;\n\
    player p2 = thing;\n\
    \n\
    label prop = 1;\n\
    \n\
    template thing\n\
        x : [0..10] init 0;\n\
        x' = x;\n\
        \n\
        label attr = 1;\n\
        \n\
        [wait] 1;\n\
    endtemplate\n";

    let atls = [
        "!",
        "prop &&",
        "foo",
        "<<p1 p2>> F (prop)",
        "<<p1, p2>> F (global.prop)",
        "[[>>",
        "()",
        "prop || p2",
        "p2.wait",
        "p1.attr && p2.attr && p3.attr",
        "p1.x > 0",
        "p1.x",
    ];

    let root = parse_lcgs(lcgs_raw).unwrap();
    let game = IntermediateLcgs::create(root).unwrap();

    for atl_raw in atls {
        let mut errors = ErrorLog::new();
        let is_none = parse_atl(atl_raw, &mut errors)
            .and_then(|expr| convert_expr_to_phi(&expr, &game, &mut errors))
            .is_none();
        assert!(
            is_none && errors.has_errors(),
            "For '{}', ErrorLog is empty",
            atl_raw
        );
    }
}
