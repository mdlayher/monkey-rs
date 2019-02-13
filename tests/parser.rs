extern crate mdl_monkey;

use mdl_monkey::{ast, lexer, parser, token};

#[test]
fn parse_statements() {
    let _prog = parse(
        "
let five = 5;
let ten = 10;
return 5;
",
    );

    // TODO(mdlayher): finish parser and tests.
}

#[test]
fn parse_identifier_expression() {
    let prog = parse("foobar;");

    assert_eq!(prog.statements.len(), 1);

    let id =
        if let ast::Statement::Expression(ast::Expression::Identifier(id)) = &prog.statements[0] {
            id.to_string()
        } else {
            panic!("not an identifier expression");
        };

    assert_eq!("foobar", id);
}

#[test]
fn parse_integer_literal_expression() {
    let prog = parse("5;");

    assert_eq!(prog.statements.len(), 1);

    let want = token::Integer {
        radix: token::Radix::Decimal,
        value: 5,
    };

    let got = if let ast::Statement::Expression(ast::Expression::Integer(int)) = &prog.statements[0]
    {
        int
    } else {
        panic!("not an integer expression");
    };

    assert_eq!(want, *got);
}

#[test]
fn parse_prefix_integer_expressions() {
    let tests = vec![
        ("!5;", token::Token::Bang, 5),
        ("-15;", token::Token::Minus, 15),
    ];

    for test in tests {
        let (input, want_op, want_int) = test;
        let prog = parse(input);

        let got =
            if let ast::Statement::Expression(ast::Expression::Prefix(pre)) = &prog.statements[0] {
                pre
            } else {
                panic!("not a prefix expression");
            };

        let got_int = if let ast::Expression::Integer(int) = &*got.right {
            int
        } else {
            panic!("not an integer expression");
        };

        assert_eq!(want_op, got.operator);
        assert_eq!(want_int, got_int.value)
    }
}

#[test]
fn parse_prefix_boolean_expressions() {
    let tests = vec![
        ("!true;", token::Token::Bang, true),
        ("!false;", token::Token::Bang, false),
    ];

    for test in tests {
        let (input, want_op, want_bool) = test;
        let prog = parse(input);

        let got =
            if let ast::Statement::Expression(ast::Expression::Prefix(pre)) = &prog.statements[0] {
                pre
            } else {
                panic!("not a prefix expression");
            };

        let got_bool = if let ast::Expression::Boolean(b) = &*got.right {
            b
        } else {
            panic!("not an integer expression");
        };

        assert_eq!(want_op, got.operator);
        assert_eq!(want_bool, *got_bool);
    }
}

#[test]
fn parse_infix_integer_expressions() {
    let int = ast::Expression::Integer(token::Integer {
        radix: token::Radix::Decimal,
        value: 5,
    });

    let tests = vec![
        ("5 + 5;", token::Token::Plus),
        ("5 - 5;", token::Token::Minus),
        ("5 * 5;", token::Token::Asterisk),
        ("5 / 5;", token::Token::Slash),
        ("5 % 5;", token::Token::Percent),
        ("5 > 5;", token::Token::GreaterThan),
        ("5 < 5;", token::Token::LessThan),
        ("5 == 5;", token::Token::Equal),
        ("5 != 5;", token::Token::NotEqual),
    ];

    for (input, want_op) in tests {
        let prog = parse(input);

        let got =
            if let ast::Statement::Expression(ast::Expression::Infix(inf)) = &prog.statements[0] {
                inf
            } else {
                panic!("not an infix expression");
            };

        assert_eq!(int, *got.left);
        assert_eq!(want_op, got.operator);
        assert_eq!(int, *got.right);
    }
}

#[test]
fn parse_infix_boolean_expressions() {
    let etrue = ast::Expression::Boolean(true);
    let efalse = ast::Expression::Boolean(false);

    let tests = vec![
        ("true == true", &etrue, token::Token::Equal, &etrue),
        ("true != false", &etrue, token::Token::NotEqual, &efalse),
        ("false == false", &efalse, token::Token::Equal, &efalse),
    ];

    for (input, want_left, want_op, want_right) in tests {
        let prog = parse(input);

        let got =
            if let ast::Statement::Expression(ast::Expression::Infix(inf)) = &prog.statements[0] {
                inf
            } else {
                panic!("not an infix expression");
            };

        assert_eq!(*want_left, *got.left);
        assert_eq!(want_op, got.operator);
        assert_eq!(*want_right, *got.right);
    }
}

#[test]
fn parse_operator_precedence() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("1 + 2 % 3", "(1 + (2 % 3))"),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    for (input, want) in tests {
        let got = format!("{}", parse(input));

        assert_eq!(want, got);
    }
}

fn parse(input: &str) -> ast::Program {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    p.parse().expect("failed to parse program")
}
