extern crate mdl_monkey;

use std::collections::{BTreeMap, BTreeSet};

use mdl_monkey::{
    ast,
    code::{BinaryOpcode, Opcode, UnaryOpcode},
    compiler, lexer,
    object::{self, Array, Hashable, Object},
    parser,
    vm::*,
};

#[test]
fn vm_run_ok() {
    let tests = vec![
        ("", Object::Null),
        ("1", Object::Integer(1)),
        ("1 + 2", Object::Integer(3)),
        ("2 - 1 - 1", Object::Integer(0)),
        ("2 * 2", Object::Integer(4)),
        ("10 / 3", Object::Integer(3)),
        ("10 % 3", Object::Integer(1)),
        ("1 * 1.0", Object::Float(1.0)),
        ("2.0 * 1", Object::Float(2.0)),
        ("1.0 + 1.0", Object::Float(2.0)),
        ("1.0 - 1.0", Object::Float(0.0)),
        ("2.0 * 1.0", Object::Float(2.0)),
        ("2.0 / 1.0", Object::Float(2.0)),
        ("4.0 % 3.0", Object::Float(1.0)),
        ("true", object::TRUE),
        ("false", object::FALSE),
        ("1 == 1", object::TRUE),
        ("1 != 1", object::FALSE),
        ("1 < 2", object::TRUE),
        ("1 > 2", object::FALSE),
        ("1.0 < 2.0", object::TRUE),
        ("1.0 > 0.9", object::TRUE),
        ("true == true", object::TRUE),
        ("true != false", object::TRUE),
        ("(1 < 2) == true", object::TRUE),
        ("-1", Object::Integer(-1)),
        ("!!true", object::TRUE),
        ("!5", object::FALSE),
        ("!!5", object::TRUE),
        ("if (true) { 10 }", Object::Integer(10)),
        ("if (true) { 10 } else { 20 }", Object::Integer(10)),
        ("if (false) { 10 } else { 20 }", Object::Integer(20)),
        ("if (1) { 10 }", Object::Integer(10)),
        ("if (1 < 2) { 10 }", Object::Integer(10)),
        ("if (1 > 2) { 10 }", Object::Null),
        ("if (false) { 10 }", Object::Null),
        ("!(if (false) { 5; })", object::TRUE),
        (
            "if ((if (false) { 10 })) { 10 } else { 20 }",
            Object::Integer(20),
        ),
        ("let one = 1; one;", Object::Integer(1)),
        ("let one = 1; let two = 2; one + two;", Object::Integer(3)),
        (
            "let one = 1; let two = one + one; one + two;",
            Object::Integer(3),
        ),
        (r#""monkey";"#, Object::String("monkey".to_string())),
        (r#""mon" + "key";"#, Object::String("monkey".to_string())),
        ("[]", Object::Array(object::Array::default())),
        (
            "[1, 2, 3]",
            Object::Array(object::Array {
                elements: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
            }),
        ),
        (
            r#"[1 + 2, "foo" + "bar"]"#,
            Object::Array(object::Array {
                elements: vec![Object::Integer(3), Object::String("foobar".to_string())],
            }),
        ),
        ("{}", Object::Hash(object::Hash::default())),
        (
            "{1: 2, 3: 4}",
            Object::Hash(object::Hash {
                pairs: {
                    let mut m = BTreeMap::new();
                    m.insert(Hashable::Integer(1), Object::Integer(2));
                    m.insert(Hashable::Integer(3), Object::Integer(4));
                    m
                },
            }),
        ),
        (
            r#"{1 + 1: 2 * 2, true: false, "hello": "world"}"#,
            Object::Hash(object::Hash {
                pairs: {
                    let mut m = BTreeMap::new();
                    m.insert(Hashable::Integer(2), Object::Integer(4));
                    m.insert(Hashable::Boolean(true), Object::Boolean(false));
                    m.insert(
                        Hashable::String("hello".to_string()),
                        Object::String("world".to_string()),
                    );
                    m
                },
            }),
        ),
        ("[1, 2, 3][1]", Object::Integer(2)),
        ("[[1, 1, 1]][0][0]", Object::Integer(1)),
        ("[][0]", Object::Null),
        ("[1][-1]", Object::Null),
        ("{1: 1, 2: 2}[1]", Object::Integer(1)),
        ("{1: 1}[0]", Object::Null),
        ("{}[true]", Object::Null),
        ("{1: {true: 2}, 2: 2}[1][true]", Object::Integer(2)),
        ("set{}", Object::Set(object::Set::default())),
        (
            "set{0, 1}",
            Object::Set(object::Set {
                set: {
                    let mut s = BTreeSet::new();
                    s.insert(Hashable::Integer(0));
                    s.insert(Hashable::Integer(1));
                    s
                },
            }),
        ),
        ("set{0}[0]", object::TRUE),
        ("set{0}[true]", object::FALSE),
        ("set{0, 1} < set{1}", object::FALSE),
        ("set{0, 1} > set{1}", object::TRUE),
        (
            "set{0} + set{1}",
            Object::Set(object::Set {
                set: {
                    let mut s = BTreeSet::new();
                    s.insert(Hashable::Integer(0));
                    s.insert(Hashable::Integer(1));
                    s
                },
            }),
        ),
        (
            "set{1, 0} - set{1}",
            Object::Set(object::Set {
                set: {
                    let mut s = BTreeSet::new();
                    s.insert(Hashable::Integer(0));
                    s
                },
            }),
        ),
        (
            "set{1, 0} * set{1}",
            Object::Set(object::Set {
                set: {
                    let mut s = BTreeSet::new();
                    s.insert(Hashable::Integer(1));
                    s
                },
            }),
        ),
        (
            "set{1, 0} / set{1, 2}",
            Object::Set(object::Set {
                set: {
                    let mut s = BTreeSet::new();
                    s.insert(Hashable::Integer(0));
                    s.insert(Hashable::Integer(2));
                    s
                },
            }),
        ),
        (
            "
                let do = fn() { 5 + 10; };
                do();
            ",
            Object::Integer(15),
        ),
        (
            "
                let one = fn() { 1; };
                let two = fn() { 2; };
                one() + two()
            ",
            Object::Integer(3),
        ),
        (
            "
                let a = fn() { 1; };
                let b = fn() { a() + 1; };
                let c = fn() { b() + 1; };
                c();
            ",
            Object::Integer(3),
        ),
        (
            "
                let early = fn() { return 99; 100; };
                early();
            ",
            Object::Integer(99),
        ),
        (
            "
                let early = fn() { return 99; return 100; };
                early();
            ",
            Object::Integer(99),
        ),
        (
            "
                let none = fn() { };
                none();
            ",
            Object::Null,
        ),
        (
            "
                let one = fn() { };
                let two = fn() { one(); };
                one();
                two();
            ",
            Object::Null,
        ),
        (
            "
                let one = fn() { 1; };
                let wrap = fn() { one; };
                wrap()();
            ",
            Object::Integer(1),
        ),
        (
            "
                let one = fn() { &1; };
                *one();
            ",
            Object::Integer(1),
        ),
        (
            "
                let a = &1;
                let b = &2;
                *(a + 1);
            ",
            Object::Integer(2),
        ),
        (
            "
                let a = &1;
                let b = &2;
                *(b - 1);
            ",
            Object::Integer(1),
        ),
        (
            "
                let a = &&&1;
                ***a;
            ",
            Object::Integer(1),
        ),
        (
            // BUG(mdlayher): let* assigns through multiple indirections.
            "
                let a = &&&1;
                let *a = 2;
                ***a;
            ",
            Object::Integer(2),
        ),
        (
            "
                let one = fn() {
                    let one = 1;
                    one
                };
                one();
            ",
            Object::Integer(1),
        ),
        (
            "
                let seed = 50;
                let one = fn() {
                    let num = 1;
                    seed - num;
                };
                let two = fn() {
                    let num = 2;
                    seed - num;
                };
                one() + two();
            ",
            Object::Integer(97),
        ),
        (
            "
                let identity = fn(a) { a };
                identity(4);
            ",
            Object::Integer(4),
        ),
        (
            "
                let sum = fn(a, b) { a + b };
                sum(1, 2);
            ",
            Object::Integer(3),
        ),
        (
            "
                let sum = fn(a, b) {
                    let c = a + b;
                    c
                };
                sum(1, 2);
            ",
            Object::Integer(3),
        ),
        (
            "
                let sum = fn(a, b) {
                    let c = a + b;
                    c
                };
                sum(1, 2) + sum(3, 4);
            ",
            Object::Integer(10),
        ),
        (
            "
                let global = 10;

                let sum = fn(a, b) {
                    let c = a + b;
                    c + global;
                };

                let outer = fn() {
                    sum(1, 2) + sum(3, 4) + global;
                };

                outer() + global;
            ",
            Object::Integer(50),
        ),
        ("first([1, 2, 3])", Object::Integer(1)),
        ("last([1, 2, 3])", Object::Integer(3)),
        (r#"len("foo")"#, Object::Integer(3)),
        (
            "push([1, 2, 3], 4)",
            Object::Array(Array {
                elements: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                ],
            }),
        ),
        (
            "rest([1, 2, 3])",
            Object::Array(Array {
                elements: vec![Object::Integer(2), Object::Integer(3)],
            }),
        ),
        (
            "
                let newClosure = fn(a, b, c) {
                    fn() { c; b; a; };
                };
                let closure = newClosure(99, 0, 0);
                closure();
            ",
            Object::Integer(99),
        ),
        (
            "
                let newAdder = fn(a, b) {
                    fn(c) { a + b + c };
                };
                let adder = newAdder(1, 2);
                adder(8);
            ",
            Object::Integer(11),
        ),
        (
            "
                let a = 1;
                let newAdderOuter = fn(b) {
                    fn(c) {
                        fn(d) { a + b + c + d };
                    };
                };
                let newAdderInner = newAdderOuter(2);
                let adder = newAdderInner(3);
                adder(8);
            ",
            Object::Integer(14),
        ),
        (
            "
                let fibonacci = fn(x) {
                    if (x == 0) {
                        return 0;
                    }
                    if (x == 1) {
                        return 1;
                    }
                    fibonacci(x - 1) + fibonacci(x - 2);
                };
                fibonacci(15);
            ",
            Object::Integer(610),
        ),
    ];

    for (input, want) in &tests {
        let bc = compile(input);
        let mut vm = Vm::new(bc.clone());
        vm.run()
            .unwrap_or_else(|err| panic!("failed to run VM: {}: {}", input, err));

        assert_eq!(
            *want,
            *vm.last_popped(),
            "\ninput: {}, incorrect value removed from stack\n\nstack: {:?}\n\nbytecode:\n\n{}",
            input,
            vm.dump_stack(),
            bc,
        );
    }
}

#[test]
fn vm_runtime_errors() {
    let tests = vec![
        (
            "1[0]",
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::Index),
                vec![Object::Integer(1), Object::Integer(0)],
            ),
        ),
        (
            "-true",
            ErrorKind::OperatorUnsupported(Opcode::Unary(UnaryOpcode::Negate), vec![object::TRUE]),
        ),
        (
            "true + false",
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::Add),
                vec![object::TRUE, object::FALSE],
            ),
        ),
        (
            "false - false",
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::Sub),
                vec![object::FALSE, object::FALSE],
            ),
        ),
        (
            "true + 1",
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::Add),
                vec![object::TRUE, Object::Integer(1)],
            ),
        ),
        (
            "1.1 == 1.1",
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::Equal),
                vec![Object::Float(1.1), Object::Float(1.1)],
            ),
        ),
        (
            "1.1 != 1.1",
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::NotEqual),
                vec![Object::Float(1.1), Object::Float(1.1)],
            ),
        ),
        (
            "{0.00: false}",
            ErrorKind::Object(object::Error::NotHashable(Object::Float(0.))),
        ),
        (
            r#"[0]["hello"]"#,
            ErrorKind::OperatorUnsupported(
                Opcode::Binary(BinaryOpcode::Index),
                vec![
                    Object::Array(object::Array {
                        elements: vec![Object::Integer(0)],
                    }),
                    Object::String("hello".to_string()),
                ],
            ),
        ),
        (
            r#"{}[0.0]"#,
            ErrorKind::Object(object::Error::NotHashable(Object::Float(0.))),
        ),
        (
            "fn() { 1; }(1)",
            ErrorKind::WrongNumberArguments { want: 0, got: 1 },
        ),
        (
            "fn(a) { a; }()",
            ErrorKind::WrongNumberArguments { want: 1, got: 0 },
        ),
        (
            "fn(a, b) { a + b; }(1)",
            ErrorKind::WrongNumberArguments { want: 2, got: 1 },
        ),
        (
            "len(1)",
            ErrorKind::Object(object::Error::Builtin(
                object::Builtin::Len,
                "argument 1 cannot be used".to_string(),
            )),
        ),
    ];

    for (input, want) in &tests {
        let mut vm = Vm::new(compile(input));
        let err = vm
            .run()
            .expect_err(&format!("input: {}, run did not return an error", input));

        if let Error::Runtime(got) = err {
            assert_eq!(*want, got);
        } else {
            panic!("not a runtime error: {:?}", err);
        }
    }
}

#[test]
fn vm_grow_stack() {
    // The VM should grow its stack as needed.
    let mut vm = Vm::with_stack_size(compile("1 + (1 + (1 + (1 + 1)))"), 0);
    vm.run().expect("failed to run VM");

    let stack = vm.dump_stack();

    // Expect the stack to have grown at least large enough to hold all 5
    // elements.
    assert!(stack.len() > 4, "stack did not grow length");
    assert!(stack.capacity() > 4, "stack did not grow capacity");
}

fn compile(input: &str) -> compiler::Bytecode {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = ast::Node::Program(p.parse().expect("failed to parse program"));

    let mut c = compiler::Compiler::new();
    c.compile(prog).expect("failed to compile");
    c.bytecode()
}
