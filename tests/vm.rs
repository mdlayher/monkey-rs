extern crate mdl_monkey;

use mdl_monkey::vm::*;
use mdl_monkey::{
    ast, compiler, lexer,
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
    ];

    for (input, want) in &tests {
        let mut stack = new_stack();
        let mut vm = Vm::new(&mut stack);
        vm.run(&compile(input)).expect("failed to run VM");

        assert_eq!(
            *want,
            *vm.last_popped(),
            "incorrect value removed from stack, debug:\n{:?}",
            vm.dump_stack(),
        );
    }
}

#[test]
fn vm_grow_stack() {
    // Start with an empty stack and make the VM grow the stack as more values
    // are added.
    let mut stack = vec![];
    let mut vm = Vm::new(&mut stack);
    vm.run(&compile("1 + (1 + (1 + (1 + 1)))"))
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
