extern crate mdl_monkey;

use mdl_monkey::vm::*;
use mdl_monkey::{
    ast,
    code::{BinaryOpcode, Opcode, UnaryOpcode},
    compiler, lexer,
    object::{self, Object},
    parser,
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
    ];

    for (input, want) in &tests {
        let mut stack = new_stack();
        let mut vm = Vm::new(&mut stack);
        vm.run(compile(input)).expect("failed to run VM");

        assert_eq!(
            *want,
            *vm.last_popped(),
            "input: {}, incorrect value removed from stack, debug:\n{:?}",
            input,
            vm.dump_stack(),
        );
    }
}

#[test]
fn vm_runtime_errors() {
    let tests = vec![
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
    ];

    for (input, want) in &tests {
        let mut stack = new_stack();
        let mut vm = Vm::new(&mut stack);
        let err = vm
            .run(compile(input))
            .expect_err("run did not return an error");

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
    let mut vm = Vm::new(&mut stack);
    vm.run(compile("1 + (1 + (1 + (1 + 1)))"))
        .expect("failed to run VM");

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
