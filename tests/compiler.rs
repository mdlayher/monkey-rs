extern crate mdl_monkey;

use mdl_monkey::compiler::*;
use mdl_monkey::{ast, code, lexer, object, parser};

#[test]
fn compiler_ok() {
    let tests = vec![
        (
            "1 + 2",
            vec![
                // 1
                code::Opcode::Constant as u8,
                0x00,
                0x00,
                // 2
                code::Opcode::Constant as u8,
                0x00,
                0x01,
                // +, pop
                code::Opcode::Add as u8,
                code::Opcode::Pop as u8,
            ],
            vec![object::Object::Integer(1), object::Object::Integer(2)],
        ),
        (
            "2; 4;",
            vec![
                // 2
                code::Opcode::Constant as u8,
                0x00,
                0x00,
                // pop
                code::Opcode::Pop as u8,
                // 4
                code::Opcode::Constant as u8,
                0x00,
                0x01,
                // pop
                code::Opcode::Pop as u8,
            ],
            vec![object::Object::Integer(2), object::Object::Integer(4)],
        ),
        (
            "true; false;",
            vec![
                // true
                code::Opcode::True as u8,
                // pop
                code::Opcode::Pop as u8,
                // false
                code::Opcode::False as u8,
                // pop
                code::Opcode::Pop as u8,
            ],
            vec![],
        ),
        (
            "2 == 4",
            vec![
                // 2
                code::Opcode::Constant as u8,
                0x00,
                0x00,
                // 4
                code::Opcode::Constant as u8,
                0x00,
                0x01,
                // equal
                code::Opcode::Equal as u8,
                // pop
                code::Opcode::Pop as u8,
            ],
            vec![object::Object::Integer(2), object::Object::Integer(4)],
        ),
        (
            "2 != 4",
            vec![
                // 2
                code::Opcode::Constant as u8,
                0x00,
                0x00,
                // 4
                code::Opcode::Constant as u8,
                0x00,
                0x01,
                // not equal
                code::Opcode::NotEqual as u8,
                // pop
                code::Opcode::Pop as u8,
            ],
            vec![object::Object::Integer(2), object::Object::Integer(4)],
        ),
        (
            "2 > 4",
            vec![
                // 2
                code::Opcode::Constant as u8,
                0x00,
                0x00,
                // 4
                code::Opcode::Constant as u8,
                0x00,
                0x01,
                // greater than
                code::Opcode::GreaterThan as u8,
                // pop
                code::Opcode::Pop as u8,
            ],
            vec![object::Object::Integer(2), object::Object::Integer(4)],
        ),
        (
            "2 < 4",
            // Compiler reorders the less-than to a greater-than operation.
            vec![
                // 4
                code::Opcode::Constant as u8,
                0x00,
                0x00,
                // 2
                code::Opcode::Constant as u8,
                0x00,
                0x01,
                // greater than
                code::Opcode::GreaterThan as u8,
                // pop
                code::Opcode::Pop as u8,
            ],
            vec![object::Object::Integer(4), object::Object::Integer(2)],
        ),
    ];

    for (input, instructions, constants) in &tests {
        let bc = compile(input);

        // Check constants first for easier debugging.
        assert_eq!(*constants, bc.constants, "unexpected constants");
        assert_instructions(instructions, &bc.instructions);
    }
}

fn assert_instructions(want: &[u8], got: &[u8]) {
    let want_ins = code::Instructions::parse(want).expect("failed to parse want instructions");
    let got_ins = code::Instructions::parse(got).expect("failed to parse got instructions");

    assert_eq!(
        want_ins, got_ins,
        "unexpected instructions stream:\nwant:\n{}\ngot:\n{}",
        want_ins, got_ins
    );
}

fn compile(input: &str) -> Bytecode {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    let prog = ast::Node::Program(p.parse().expect("failed to parse program"));

    let mut c = Compiler::new();
    c.compile(prog).expect("failed to compile");
    c.bytecode()
}
