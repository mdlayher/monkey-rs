extern crate mdl_monkey;

use mdl_monkey::{ast, code::*, compiler::*, lexer, object::Object, parser};

#[test]
fn compiler_ok() {
    let tests = vec![
        (
            "1 + 2",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // +, pop
                BinaryOpcode::Add as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "2; 4;",
            vec![
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                ControlOpcode::Pop as u8,
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "true; false;",
            vec![
                // true
                ControlOpcode::True as u8,
                ControlOpcode::Pop as u8,
                // false
                ControlOpcode::False as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![],
        ),
        (
            "2 == 4",
            vec![
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // equal
                BinaryOpcode::Equal as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "2 != 4",
            vec![
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // not equal
                BinaryOpcode::NotEqual as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "2 > 4",
            vec![
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // greater than
                BinaryOpcode::GreaterThan as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "2 < 4",
            // Compiler reorders the less-than to a greater-than operation.
            vec![
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // greater than
                BinaryOpcode::GreaterThan as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(4), Object::Integer(2)],
        ),
        (
            "-1; -2;",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // negate
                UnaryOpcode::Negate as u8,
                ControlOpcode::Pop as u8,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // negate
                UnaryOpcode::Negate as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "!true; !!false;",
            vec![
                // true
                ControlOpcode::True as u8,
                // not
                UnaryOpcode::Not as u8,
                ControlOpcode::Pop as u8,
                // false
                ControlOpcode::False as u8,
                // not, not
                UnaryOpcode::Not as u8,
                UnaryOpcode::Not as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![],
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
    let want_ins = Instructions::parse(want).expect("failed to parse want instructions");
    let got_ins = Instructions::parse(got).expect("failed to parse got instructions");

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
