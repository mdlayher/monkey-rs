extern crate mdl_monkey;

use mdl_monkey::{ast, evaluator, lexer, object, parser};

use std::collections::BTreeMap;

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
fn evaluate_float_expression() {
    let tests = vec![
        ("0.1", 0.1),
        ("-1.0 + 0.1", -0.9),
        ("1.0 - 1.0", 0.0),
        ("1.0 * 2.0", 2.0),
        ("0.1 / 10.0", 0.01),
        ("4.0 % 3.0", 1.0),
        ("1 + 0.1", 1.1),
        ("2 - 0.1", 1.9),
        ("1.0 * 3.0", 3.0),
        ("4 / 1.0", 4.0),
        ("5 % 3.0", 2.0),
    ];

    for (input, want) in tests {
        let got = if let object::Object::Float(float) = eval(input) {
            float
        } else {
            panic!("not a float object");
        };

        assert!((want - got).abs() < std::f64::EPSILON);
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
fn evaluate_string_expression() {
    let tests = vec![r#""hello world""#, r#""hello" + " " + "world""#];

    for input in tests {
        let got = if let object::Object::String(s) = eval(input) {
            s
        } else {
            panic!("not a string object");
        };

        assert_eq!("hello world", got);
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
        // Closures should evaluate properly, and with the correct variable
        // scopes.
        (
            "
let x = 0;
let f = fn(n) {
    x + n
};
let x = 1;
f(1)
",
            1,
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

#[test]
fn evaluate_function_application_error() {
    // Try to invoke add with one parameter in the body of apply.
    let err = eval_result(
        "
let add = fn(x, y) { x + y };
let apply = fn(func, x, y) { func(x) };
apply(add, 2, 2);
",
    )
    .expect_err("expected an error but none was found");

    if let evaluator::Error::Evaluation(node, _) = err {
        // Pinpoint the error.
        assert_eq!("func(x)", format!("{}", node));
    } else {
        panic!("expected evaluation error");
    }
}

#[test]
fn evaluate_builtin_first() {
    let tests = vec![
        ("first([1, 2, 3])", object::Object::Integer(1)),
        (
            r#"first(["hello", "world"])"#,
            object::Object::String("hello".to_string()),
        ),
        ("first([])", object::Object::Null),
    ];

    for (input, want) in tests {
        let got = eval(input);
        match got {
            object::Object::Integer(_) | object::Object::String(_) | object::Object::Null => {}
            _ => panic!("unexpected result object"),
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_builtin_first_errors() {
    let tests = vec!["first()", r#"first("foo", "bar")"#];

    for input in tests {
        let err = eval_result(input).expect_err("expected an error but none was found");

        if let evaluator::Error::Object(object::Error::Builtin(b, _)) = err {
            assert_eq!(object::Builtin::First, b);
        } else {
            panic!("not a built-in object error");
        }
    }
}

#[test]
fn evaluate_builtin_last() {
    let tests = vec![
        ("last([1, 2, 3])", object::Object::Integer(3)),
        (
            r#"last(["hello", "world"])"#,
            object::Object::String("world".to_string()),
        ),
        ("last([])", object::Object::Null),
    ];

    for (input, want) in tests {
        let got = eval(input);
        match got {
            object::Object::Integer(_) | object::Object::String(_) | object::Object::Null => {}
            _ => panic!("unexpected result object"),
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_builtin_last_errors() {
    let tests = vec!["last()", r#"last("foo", "bar")"#];

    for input in tests {
        let err = eval_result(input).expect_err("expected an error but none was found");

        if let evaluator::Error::Object(object::Error::Builtin(b, _)) = err {
            assert_eq!(object::Builtin::Last, b);
        } else {
            panic!("not a built-in object error");
        }
    }
}

#[test]
fn evaluate_builtin_len() {
    let tests = vec![
        (r#"len("")"#, 0),
        (r#"len("four")"#, 4),
        (r#"len("hello world")"#, 11),
        ("len([1, 2, 3])", 3),
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
fn evaluate_builtin_len_errors() {
    let tests = vec![r#"len()"#, r#"len(1)"#, r#"len("foo", "bar")"#];

    for input in tests {
        let err = eval_result(input).expect_err("expected an error but none was found");

        if let evaluator::Error::Object(object::Error::Builtin(b, _)) = err {
            assert_eq!(object::Builtin::Len, b);
        } else {
            panic!("not a built-in object error");
        }
    }
}

#[test]
fn evaluate_builtin_push() {
    let tests = vec![
        (
            "push([1, 2, 3], 4)",
            object::Array {
                elements: vec![
                    object::Object::Integer(1),
                    object::Object::Integer(2),
                    object::Object::Integer(3),
                    object::Object::Integer(4),
                ],
            },
        ),
        (
            r#"push(["hello"], "world")"#,
            object::Array {
                elements: vec![
                    object::Object::String("hello".to_string()),
                    object::Object::String("world".to_string()),
                ],
            },
        ),
        // TODO(mdlayher): fix empty arrays.
        (
            "push([], 1)",
            object::Array {
                elements: vec![object::Object::Integer(1)],
            },
        ),
    ];

    for (input, want) in tests {
        if let object::Object::Array(a) = eval(input) {
            assert_eq!(want, a);
        } else {
            panic!("push did not return an array");
        }
    }
}

#[test]
fn evaluate_builtin_push_errors() {
    let tests = vec!["push()", "push(1)", r#"push("foo", "bar")"#];

    for input in tests {
        let err = eval_result(input).expect_err("expected an error but none was found");

        if let evaluator::Error::Object(object::Error::Builtin(b, _)) = err {
            assert_eq!(object::Builtin::Push, b);
        } else {
            panic!("not a built-in object error");
        }
    }
}

#[test]
fn evaluate_builtin_rest() {
    let tests = vec![
        (
            "rest([1, 2, 3])",
            object::Array {
                elements: vec![object::Object::Integer(2), object::Object::Integer(3)],
            },
        ),
        (
            r#"rest(["hello", "world"])"#,
            object::Array {
                elements: vec![object::Object::String("world".to_string())],
            },
        ),
        (
            "rest(rest(rest([1, 2, 3, 4])))",
            object::Array {
                elements: vec![object::Object::Integer(4)],
            },
        ),
        // TODO(mdlayher): fix empty arrays.
        ("rest([0])", object::Array { elements: vec![] }),
    ];

    for (input, want) in tests {
        if let object::Object::Array(a) = eval(input) {
            assert_eq!(want, a);
        } else {
            panic!("rest did not return an array");
        }
    }
}

#[test]
fn evaluate_builtin_rest_errors() {
    let tests = vec!["rest()", r#"rest("foo", "bar")"#];

    for input in tests {
        let err = eval_result(input).expect_err("expected an error but none was found");

        if let evaluator::Error::Object(object::Error::Builtin(b, _)) = err {
            assert_eq!(object::Builtin::Rest, b);
        } else {
            panic!("not a built-in object error");
        }
    }
}

#[test]
fn evaluate_array_object() {
    let got = if let object::Object::Array(arr) = eval("[1, 2 * 2, 3 + 3]") {
        arr
    } else {
        panic!("not an array object");
    };

    let want = vec![
        object::Object::Integer(1),
        object::Object::Integer(4),
        object::Object::Integer(6),
    ];

    assert_eq!(want.len(), got.elements.len());
    for e in want.iter().zip(got.elements.iter()) {
        assert_eq!(e.0, e.1);
    }
}

#[test]
fn evaluate_array_index_objects() {
    let tests = vec![
        ("[1, 2, 3][0]", object::Object::Integer(1)),
        ("[1, 2, 3][-1]", object::Object::Null),
        ("[2, 4, 6][4 - 4 * 1 + 2]", object::Object::Integer(6)),
        (
            r#"["hello", "world", fn(x) { x }][2](1)"#,
            object::Object::Integer(1),
        ),
    ];

    for (input, want) in tests {
        let got = eval(input);
        match got {
            object::Object::Integer(_) | object::Object::Null => {}
            _ => panic!("unexpected result object"),
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_index_objects_errors() {
    let tests = vec!["1[0]", "[1][true]", r#"1["hello"]"#, "[0][fn(x) { x }]"];

    for input in tests {
        let err = eval_result(input).expect_err("expected an error but none was found");

        if let evaluator::Error::Evaluation(_, _) = err {
            continue;
        } else {
            panic!("not an evaluation error");
        }
    }
}

#[test]
fn evaluate_hash_objects() {
    let mut pairs = BTreeMap::new();
    pairs.insert(
        object::Hashable::String("one".to_string()),
        object::Object::Integer(1),
    );
    pairs.insert(
        object::Hashable::Integer(2),
        object::Object::String("two".to_string()),
    );
    pairs.insert(object::Hashable::Boolean(true), object::Object::Integer(3));

    let tests = vec![(
        r#"
let two = 2;
{"one": 1 + (0 * 1), two: "two", true: 3}
        "#,
        object::Hash { pairs },
    )];

    for (input, want) in tests {
        let got = if let object::Object::Hash(h) = eval(input) {
            h
        } else {
            panic!("not a hash object");
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_hash_index_objects() {
    let tests = vec![
        (r#"{"foo": 1}["bar"]"#, object::Object::Null),
        (r#"{"foo": 1, "bar": 0}["foo"]"#, object::Object::Integer(1)),
        (r#"{false: 1, true: 0}[false]"#, object::Object::Integer(1)),
        (r#"{10: 1, 1: 0}[10]"#, object::Object::Integer(1)),
        (
            r#"{1: 1, 0: 0}[fn(x) { x }(1)]"#,
            object::Object::Integer(1),
        ),
    ];

    for (input, want) in tests {
        assert_eq!(want, eval(input));
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
