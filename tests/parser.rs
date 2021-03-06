extern crate mdl_monkey;

use mdl_monkey::{ast, lexer, parser, token};

#[test]
fn parse_statements() {
    // Good enough!
    let prog =
        parse("let x = 0x005 % (0.1 + 6) + add(1, 2, mult(10, fn(x, y) { return x * y; }(2, 2)));");

    let want =
        "let x = ((0x5 % (0.1 + 6)) + add(1, 2, mult(10, fn(x, y) { return (x * y); }(2, 2))));";

    assert_eq!(want, format!("{}", prog));
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
fn parse_float_literal_expression() {
    let prog = parse("0.1;");

    assert_eq!(prog.statements.len(), 1);

    let want = 0.1;

    let got = if let ast::Statement::Expression(ast::Expression::Float(f)) = prog.statements[0] {
        f
    } else {
        panic!("not a float expression");
    };

    // Direct equality comparison of floats isn't a good idea, see:
    // https://github.com/rust-lang/rust-clippy/issues/46.
    let float: f64 = got.into();
    assert!((want - float).abs() < std::f64::EPSILON);
}

#[test]
fn parse_string_expression() {
    let prog = parse(r#""hello world""#);

    assert_eq!(prog.statements.len(), 1);

    let want = "hello world";

    let got = if let ast::Statement::Expression(ast::Expression::String(s)) = &prog.statements[0] {
        s
    } else {
        panic!("not a string expression");
    };

    assert_eq!(want, got);
}

#[test]
fn parse_array_literal() {
    let tests = vec![
        ("[1, 2 * 2, !true]", vec!["1", "(2 * 2)", "(!true)"]),
        ("[]", vec![]),
    ];

    for (input, want) in tests {
        let prog = parse(input);

        assert_eq!(prog.statements.len(), 1);

        let got = if let ast::Statement::Expression(ast::Expression::Array(a)) = &prog.statements[0]
        {
            a
        } else {
            panic!("not an array literal expression");
        };

        assert_eq!(got.elements.len(), want.len());
        for e in want.iter().zip(got.elements.iter()) {
            assert_eq!(*e.0, format!("{}", e.1));
        }
    }
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
            panic!("not a boolean expression");
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
fn parse_if_expressions() {
    let tests = vec!["if (x < y) { x }", "if (x > y) { x } else { y }"];

    for test in tests {
        let got = format!("{}", parse(test));

        assert_eq!(test, got);
    }
}

#[test]
fn parse_function_literals() {
    let tests = vec![
        ("fn() {};", "fn() {  }"),
        ("fn(x) { };", "fn(x) {  }"),
        ("fn(x, y) { x + y; };", "fn(x, y) { (x + y) }"),
    ];

    for (input, want) in tests {
        let got = format!("{}", parse(input));

        assert_eq!(want, got);
    }
}

#[test]
fn parse_call_expressions() {
    let tests = vec![
        ("add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
    ];

    for (input, want) in tests {
        let got = format!("{}", parse(input));

        assert_eq!(want, got);
    }
}

#[test]
fn parse_hash_expressions() {
    let tests = vec![
        (
            r#"{"one": 1, "two": 2, "three": 3}"#,
            r#"{"one": 1, "two": 2, "three": 3}"#,
        ),
        (
            r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#,
            r#"{"one": (0 + 1), "two": (10 - 8), "three": (15 / 5)}"#,
        ),
        ("{}", "{}"),
        // Duplicate keys permitted by the parser.
        ("{1: 1, 1: 1}", "{1: 1, 1: 1}"),
    ];

    for (input, want) in tests {
        let got = format!("{}", parse(input));

        assert_eq!(want, got);
    }
}

#[test]
fn parse_set_expressions() {
    let tests = vec![
        (r#"set{1, "foo", true}"#, r#"set{1, "foo", true}"#),
        ("set{1}", "set{1}"),
        // Duplicate elements permitted by the parser.
        ("set{1, 1, 1, 2}", "set{1, 1, 1, 2}"),
    ];

    for (input, want) in tests {
        let got = format!("{}", parse(input));

        assert_eq!(want, got);
    }
}

#[test]
fn parse_pointer_expressions() {
    let tests = vec![("&1", "(&1)"), ("&&1", "(&(&1))"), ("*&1", "(*(&1))")];

    for (input, want) in tests {
        let got = format!("{}", parse(input));

        assert_eq!(want, got);
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
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        ),
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
