extern crate mdl_monkey;

use std::collections::{BTreeMap, BTreeSet};

use mdl_monkey::{
    ast,
    code::{self, BinaryOpcode, Opcode, UnaryOpcode},
    compiler, lexer,
    object::{self, Hashable, Object},
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
    ];

    for (input, want) in &tests {
        let mut stack = new_stack();
        let bc = compile(input);
        let mut vm = Vm::new(&mut stack, bc.clone());
        vm.run().expect("failed to run VM");

        assert_eq!(
            *want,
            *vm.last_popped(),
            "\ninput: {}, incorrect value removed from stack\n\nstack: {:?}\n\nconstants: {:?}\n\nbytecode:\n\n{}",
            input,
            vm.dump_stack(),
            bc.constants,
            code::Instructions::parse(&bc.instructions).expect("must parse"),
        );
    }
}

#[test]
fn vm_runtime_errors() {
    let tests = vec![
        (
            "1[0]",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::Index),
                args: vec![Object::Integer(1), Object::Integer(0)],
            },
        ),
        (
            "-true",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::UnaryOperatorUnsupported,
                op: Opcode::Unary(UnaryOpcode::Negate),
                args: vec![object::TRUE],
            },
        ),
        (
            "true + false",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::Add),
                args: vec![object::TRUE, object::FALSE],
            },
        ),
        (
            "false - false",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::Sub),
                args: vec![object::FALSE, object::FALSE],
            },
        ),
        (
            "true + 1",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::Add),
                args: vec![object::TRUE, Object::Integer(1)],
            },
        ),
        (
            "1.1 == 1.1",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::Equal),
                args: vec![Object::Float(1.1), Object::Float(1.1)],
            },
        ),
        (
            "1.1 != 1.1",
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::NotEqual),
                args: vec![Object::Float(1.1), Object::Float(1.1)],
            },
        ),
        (
            "{0.00: false}",
            ErrorKind::Object(object::Error::NotHashable(Object::Float(0.))),
        ),
        (
            r#"[0]["hello"]"#,
            ErrorKind::BadArguments {
                kind: BadArgumentKind::BinaryOperatorUnsupported,
                op: Opcode::Binary(BinaryOpcode::Index),
                args: vec![
                    Object::Array(object::Array {
                        elements: vec![Object::Integer(0)],
                    }),
                    Object::String("hello".to_string()),
                ],
            },
        ),
        (
            r#"{}[0.0]"#,
            ErrorKind::Object(object::Error::NotHashable(Object::Float(0.))),
        ),
    ];

    for (input, want) in &tests {
        let mut stack = new_stack();
        let mut vm = Vm::new(&mut stack, compile(input));
        let err = vm.run().expect_err("run did not return an error");

        if let Error::Runtime(got) = err {
            assert_eq!(*want, got);
        } else {
            panic!("not a runtime error: {:?}", err);
        }
    }
}

#[test]
fn vm_grow_stack() {
    // Start with an empty stack and make the VM grow the stack as more values
    // are added.
    let mut stack = vec![];
    let mut vm = Vm::new(&mut stack, compile("1 + (1 + (1 + (1 + 1)))"));
    vm.run().expect("failed to run VM");

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
