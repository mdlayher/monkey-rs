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

#[test]
fn evaluate_return_statement() {
    let tests = vec![
        "return 10;",
        "return 10; 9;",
        "return 2 * 5; 9",
        "9; return 2 * 5; 9;",
        "if (10 > 1) {
            if (10 > 1) {
                return 10;
            }

            return 1;
        }",
    ];

    for input in tests {
        let got = if let object::Object::Integer(int) = eval(input) {
            int
        } else {
            panic!("not an integer object");
        };

        assert_eq!(10, got);
    }
}

#[test]
fn evaluate_let_statement() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c", 15),
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
fn evaluate_let_statement_unknown_identifier() {
    let input = "foobar";

    let err = eval_result(input).expect_err("expected an error but none was found");

    if let evaluator::Error::UnknownIdentifier(id) = err {
        assert_eq!(input, id);
    } else {
        panic!("not an unknown identifier error");
    }
}

#[test]
fn evaluate_function_object() {
    let input = "fn(x) { x + 2 };";

    let got = if let object::Object::Function(func) = eval(input) {
        func
    } else {
        panic!("not a function object");
    };

    assert_eq!(1, got.parameters.len());
    assert_eq!("x", got.parameters[0]);

    assert_eq!("(x + 2)", format!("{}", got.body));
}

#[test]
fn evaluate_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; } add(5, 5);", 10),
        ("fn(x) { x; }(5)", 5),
        // Closures also work!
        (
            "
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
",
            4,
        ),
        // And higher-order functions!
        (
            "
let add = fn(x, y) { x + y };
let apply = fn(func, x, y) { func(x, y) };
apply(add, 2, 2);
",
            4,
        ),
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

fn eval(input: &str) -> object::Object {
    eval_result(input).expect("failed to evaluate program")
}

fn eval_result(input: &str) -> evaluator::Result<object::Object> {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = p.parse().expect("failed to parse program");

    evaluator::eval(ast::Node::Program(prog), &mut object::Environment::new())
}
