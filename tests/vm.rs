extern crate mdl_monkey;

use mdl_monkey::vm::*;
use mdl_monkey::{ast, compiler, lexer, object, parser};

#[test]
fn vm_run_ok() {
    let tests = vec![
        ("", object::Object::Null),
        ("1", object::Object::Integer(1)),
        ("1 + 2", object::Object::Integer(3)),
        ("2 - 1 - 1", object::Object::Integer(0)),
        ("2 * 2", object::Object::Integer(4)),
        ("10 / 3", object::Object::Integer(3)),
        ("10 % 3", object::Object::Integer(1)),
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

fn compile(input: &str) -> compiler::Bytecode {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = ast::Node::Program(p.parse().expect("failed to parse program"));

    let mut c = compiler::Compiler::new();
    c.compile(prog).expect("failed to compile");
    c.bytecode()
}
