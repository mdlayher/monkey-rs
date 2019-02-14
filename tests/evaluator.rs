extern crate mdl_monkey;

use mdl_monkey::{ast, evaluator, lexer, object, parser};

#[test]
fn evaluate_integer_expression() {
    let tests = vec![("5", 5), ("10", 10)];

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
    let tests = vec![("true", true), ("false", false)];

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

fn eval(input: &str) -> object::Object {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = p.parse().expect("failed to parse program");

    evaluator::eval(ast::Node::Program(prog)).expect("failed to evaluate program")
}
