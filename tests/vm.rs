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
        ("true", object::TRUE),
        ("false", object::FALSE),
        ("1 == 1", object::TRUE),
        ("1 != 1", object::FALSE),
        ("1 < 2", object::TRUE),
        ("1 > 2", object::FALSE),
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

fn compile(input: &str) -> compiler::Bytecode {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = ast::Node::Program(p.parse().expect("failed to parse program"));

    let mut c = compiler::Compiler::new();
    c.compile(prog).expect("failed to compile");
    c.bytecode()
}
