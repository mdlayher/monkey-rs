extern crate mdl_monkey;

use mdl_monkey::{ast, evaluator, lexer, object, parser};

#[test]
fn evaluate_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 *2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ("4 % 3", 1),
    ];

    for (input, want) in tests {
        let got = if let object::Object::Integer(int) = eval(input) {
            int
        } else {
            panic!("not an integer object");
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 == 1) == true", true),
        ("(1 != 2) == true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, want) in tests {
        let got = if let object::Object::Boolean(b) = eval(input) {
            b
        } else {
            panic!("not a boolean object");
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_prefix_expression() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, want) in tests {
        let got = if let object::Object::Boolean(b) = eval(input) {
            b
        } else {
            panic!("not a boolean object");
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_if_expression() {
    let ten = object::Object::Integer(10);
    let twenty = object::Object::Integer(20);
    let null = object::Object::Null;

    let tests = vec![
        ("if (true) { 10 }", &ten),
        ("if (false) { 10 }", &null),
        ("if (1) { 10 }", &ten),
        ("if (1 < 2) { 10 }", &ten),
        ("if (1 > 2) { 10 }", &null),
        ("if (1 < 2) { 10 } else { 20 }", &ten),
        ("if (1 > 2) { 10 } else { 20 }", &twenty),
    ];

    for (input, want) in tests {
        assert_eq!(want, &eval(input));
    }
}

fn eval(input: &str) -> object::Object {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = p.parse().expect("failed to parse program");

    evaluator::eval(ast::Node::Program(prog)).expect("failed to evaluate program")
}
