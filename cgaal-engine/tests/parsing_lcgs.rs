use cgaal_engine::game_structure::{PropIdx, INVALID_IDX};
use cgaal_engine::parsing::ast::*;
use cgaal_engine::parsing::errors::ErrorLog;
use cgaal_engine::parsing::lexer::*;
use cgaal_engine::parsing::parser::*;
use cgaal_engine::parsing::span::*;

#[test]
fn const_decl_001() {
    let input = "const foo = 1";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .const_decl()
        .expect("Failed to parse valid const decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 13),
            Ident::new(Span::new(6, 9), "foo".into()),
            DeclKind::Const(Expr::new(Span::new(12, 13), ExprKind::Num(1),).into())
        )
    );
}

#[test]
fn state_label_decl_001() {
    let input = "label foo = 1";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .state_label_decl()
        .expect("Failed to parse valid state label decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 13),
            Ident::new(Span::new(6, 9), "foo".into()),
            DeclKind::StateLabel(
                PropIdx(INVALID_IDX),
                Expr::new(Span::new(12, 13), ExprKind::Num(1),)
            )
        )
    );
}

#[test]
fn state_var_decl_001() {
    let input = "foo : [0..3] init 1; foo' = 2";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .state_var_decl()
        .expect("Failed to parse valid state var decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 29),
            Ident::new(Span::new(0, 3), "foo".into()),
            DeclKind::StateVar(
                StateVarDecl::new(
                    RangeClause::new(
                        Span::new(6, 12),
                        Expr::new(Span::new(7, 8), ExprKind::Num(0),).into(),
                        Expr::new(Span::new(10, 11), ExprKind::Num(3),).into(),
                    ),
                    Expr::new(Span::new(18, 19), ExprKind::Num(1),).into(),
                    Ident::new(Span::new(21, 24), "foo".into()),
                    Expr::new(Span::new(28, 29), ExprKind::Num(2),).into(),
                )
                .into()
            )
        )
    );
}

#[test]
fn player_decl_001() {
    let input = "player foo = bar";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .player_decl()
        .expect("Failed to parse valid player decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 16),
            Ident::new(Span::new(7, 10), "foo".into()),
            DeclKind::Player(
                PlayerDecl::new(Ident::new(Span::new(13, 16), "bar".into()), Vec::new(),).into()
            )
        )
    )
}

#[test]
fn player_decl_002() {
    let input = "player bar = baz [one=1, two=2 || 3]";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .player_decl()
        .expect("Failed to parse valid player decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 36),
            Ident::new(Span::new(7, 10), "bar".into()),
            DeclKind::Player(
                PlayerDecl::new(
                    Ident::new(Span::new(13, 16), "baz".into()),
                    vec![
                        RelabelCase::new(
                            Span::new(18, 23),
                            Ident::new(Span::new(18, 21), "one".into()),
                            Expr::new(Span::new(22, 23), ExprKind::Num(1),).into(),
                        ),
                        RelabelCase::new(
                            Span::new(25, 35),
                            Ident::new(Span::new(25, 28), "two".into()),
                            Expr::new(
                                Span::new(29, 35),
                                ExprKind::Binary(
                                    BinaryOpKind::Or,
                                    Expr::new(Span::new(29, 30), ExprKind::Num(2)).into(),
                                    Expr::new(Span::new(34, 35), ExprKind::Num(3)).into()
                                ),
                            )
                            .into(),
                        ),
                    ]
                )
                .into()
            )
        )
    )
}

#[test]
fn template_decl_001() {
    let input = "template foo endtemplate";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .template_decl()
        .expect("Failed to parse valid template decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 24),
            Ident::new(Span::new(9, 12), "foo".into()),
            DeclKind::Template(Vec::new()),
        )
    )
}

#[test]
fn template_decl_002() {
    let input = "template foo label bar = 1; [act] 2; endtemplate";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .template_decl()
        .expect("Failed to parse valid template decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 48),
            Ident::new(Span::new(9, 12), "foo".into()),
            DeclKind::Template(vec![
                Decl::new(
                    Span::new(13, 26),
                    Ident::new(Span::new(19, 22), "bar".into()),
                    DeclKind::StateLabel(
                        PropIdx(INVALID_IDX),
                        Expr::new(Span::new(25, 26), ExprKind::Num(1),)
                    )
                ),
                Decl::new(
                    Span::new(28, 35),
                    Ident::new(Span::new(29, 32), "act".into()),
                    DeclKind::Action(Expr::new(Span::new(34, 35), ExprKind::Num(2),).into())
                ),
            ]),
        )
    )
}

#[test]
fn action_decl_001() {
    let input = "[act] 1";
    let errors = ErrorLog::new();
    let lexer = Lexer::new(input.as_bytes(), &errors);
    let mut parser = Parser::new(lexer, &errors);
    let decl = parser
        .action_decl()
        .expect("Failed to parse valid action decl");
    assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    assert_eq!(
        decl,
        Decl::new(
            Span::new(0, 7),
            Ident::new(Span::new(1, 4), "act".into()),
            DeclKind::Action(Expr::new(Span::new(6, 7), ExprKind::Num(1),).into())
        )
    )
}

#[test]
fn lcgs_batch() {
    let models = vec![
        "// Comment
        player p1 = thing;
        player p2 = thing;
        label prop = 1;
        template thing
            x : [0..10] init 0;
            x' = x;
            label attr = 1;
            [wait] 1;
        endtemplate",
        // ----------------------------
        "const N = 3;
        player p1 = robot;
        player p2 = robot;
        label prop = 1;
        steps : [0..100] init 0;
        steps' = steps || steps;
        template robot
            /* Multi-line
            comment // test
            /* nested btw */*/
            x : [0..N] init 0;
            x' = x;
            [wait] 1;
            [left] 1;
            [right] 1;
        endtemplate",
    ];

    for (i, model) in models.iter().enumerate() {
        let errors = ErrorLog::new();
        let lexer = Lexer::new(model.as_bytes(), &errors);
        let mut parser = Parser::new(lexer, &errors);
        let _ = parser
            .lcgs_root()
            .expect(&format!("Failed to parse valid LCGS model (index {})", i));
        parser.expect_end();
        assert!(
            errors.is_empty(),
            "ErrorLog is not empty (model index {}): {:?}",
            i,
            errors
        );
    }
}

#[test]
fn lcgs_erroneous_batch() {
    let models = vec![
        "player p1 = thing;
        label prop = 1
        template thing
            x : [0..10] init 0;
            x' = x;
            label attr = 1;
            [wait] 1;
        endtemplate",
        // ----------------------------
        "const N = 3;
        player p1 = robot [5=test];
        label prop = 1;
        template robot
            [wait] 1;
            [left] 1;
        endtemplate
        steps : [0..100] init 0;
        steps' = steps || steps;",
        // ----------------------------
        "const N = 3;
        player p1 = robot;
        label prop = 1;
        steps : [0..100] init 0;
        template robot
            [left] 1;
        endtemplate",
        // ----------------------------
        "const N = 3;
        /* unclosed comment",
        // ----------------------------
        "label prop = 1;\
        /* /* nested must be closed */\
        player p1 = robot;",
    ];

    for (i, model) in models.iter().enumerate() {
        let errors = ErrorLog::new();
        let lexer = Lexer::new(model.as_bytes(), &errors);
        let mut parser = Parser::new(lexer, &errors);
        let _ = parser.lcgs_root();
        parser.expect_end();
        assert!(
            !errors.is_empty(),
            "ErrorLog is empty. Expected error (model index {})",
            i
        );
    }
}
