extern crate mdl_monkey;

use mdl_monkey::{
    ast,
    code::*,
    compiler::*,
    lexer,
    object::{self, Object},
    parser,
};

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
            "1 * 1.0",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 1.0
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // *, pop
                BinaryOpcode::Mul as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1), Object::Float(1.0)],
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
        (
            "if (true) { 10 }; 3333;",
            vec![
                // true
                ControlOpcode::True as u8,
                // jump not true
                ControlOpcode::JumpNotTrue as u8,
                0x00,
                0x0a,
                // 10
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // jump
                ControlOpcode::Jump as u8,
                0x00,
                0x0b,
                // null
                ControlOpcode::Null as u8,
                ControlOpcode::Pop as u8,
                // 3333
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(10), Object::Integer(3333)],
        ),
        (
            "if (true) { 10 } else { 20 }; 3333;",
            vec![
                // true
                ControlOpcode::True as u8,
                // jump not true
                ControlOpcode::JumpNotTrue as u8,
                0x00,
                0x0a,
                // 10
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // jump
                ControlOpcode::Jump as u8,
                0x00,
                0x0d,
                // 20
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                ControlOpcode::Pop as u8,
                // 3333
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(10),
                Object::Integer(20),
                Object::Integer(3333),
            ],
        ),
        (
            "let one = 1; let two = 2;",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // set 1
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // set 2
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x01,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "let one = 1; one;",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // set 1
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                // get 1
                ControlOpcode::GetGlobal as u8,
                0x00,
                0x00,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            "let one = 1; let two = one; two;",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // set one
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                // get one
                ControlOpcode::GetGlobal as u8,
                0x00,
                0x00,
                // set two
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x01,
                // get two
                ControlOpcode::GetGlobal as u8,
                0x00,
                0x01,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            r#""monkey""#,
            vec![
                // monkey
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::String("monkey".to_string())],
        ),
        (
            r#""mon" + "key""#,
            vec![
                // mon
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // key
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // +
                BinaryOpcode::Add as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::String("mon".to_string()),
                Object::String("key".to_string()),
            ],
        ),
        (
            "[]",
            vec![
                // array
                CompositeOpcode::Array as u8,
                0x00,
                0x00,
                ControlOpcode::Pop as u8,
            ],
            vec![],
        ),
        (
            "[1, 2, 3]",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // 3
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                // array
                CompositeOpcode::Array as u8,
                0x00,
                0x03,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
        ),
        (
            "[1 + 2, 3 - 4]",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // +
                BinaryOpcode::Add as u8,
                // 3
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x03,
                // -
                BinaryOpcode::Sub as u8,
                // array
                CompositeOpcode::Array as u8,
                0x00,
                0x02,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
            ],
        ),
        (
            "{}",
            vec![
                // hash
                CompositeOpcode::Hash as u8,
                0x00,
                0x00,
                ControlOpcode::Pop as u8,
            ],
            vec![],
        ),
        (
            "{1: 2, 3: 4}",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // 3
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                // 4
                ControlOpcode::Constant as u8,
                0x00,
                0x03,
                // hash
                CompositeOpcode::Hash as u8,
                0x00,
                0x04,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
            ],
        ),
        (
            "[][0]",
            vec![
                // array
                CompositeOpcode::Array as u8,
                0x00,
                0x00,
                // 0
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // index
                BinaryOpcode::Index as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(0)],
        ),
        (
            r#"{}["foo"]"#,
            vec![
                // hash
                CompositeOpcode::Hash as u8,
                0x00,
                0x00,
                // foo
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // index
                BinaryOpcode::Index as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::String("foo".to_string())],
        ),
        (
            "set{0, 1}",
            vec![
                // 0
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // set
                CompositeOpcode::Set as u8,
                0x00,
                0x02,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(0), Object::Integer(1)],
        ),
        (
            "set{}[0]",
            vec![
                // set
                CompositeOpcode::Set as u8,
                0x00,
                0x00,
                // 0
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                BinaryOpcode::Index as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(0)],
        ),
        (
            "fn() { return 5 + 10 }",
            vec![
                // function
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 5
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x00,
                        // 10
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x01,
                        // add, return
                        BinaryOpcode::Add as u8,
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 0,
                }),
            ],
        ),
        (
            "fn() { 5 + 10 }",
            vec![
                // function
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 5
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x00,
                        // 10
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x01,
                        // add, return
                        BinaryOpcode::Add as u8,
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 0,
                }),
            ],
        ),
        (
            "fn() { 1; 2 }",
            vec![
                // function
                ControlOpcode::Constant as u8,
                0x00,
                0x02,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 1
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x00,
                        ControlOpcode::Pop as u8,
                        // 2
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x01,
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 0,
                }),
            ],
        ),
        (
            "fn() { }",
            vec![
                // function
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::CompiledFunction(object::CompiledFunction {
                instructions: vec![ControlOpcode::Return as u8],
                num_locals: 0,
            })],
        ),
        (
            "fn() { 24 }();",
            vec![
                // function
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // call
                ControlOpcode::Call as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(24),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 24
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x00,
                        // return
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 0,
                }),
            ],
        ),
        (
            "let noArg = fn() { 24 }; noArg();",
            vec![
                // function
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // noArg bindings
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                ControlOpcode::GetGlobal as u8,
                0x00,
                0x00,
                // call
                ControlOpcode::Call as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(24),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 24
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x00,
                        // return
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 0,
                }),
            ],
        ),
        (
            "let one = &1; *one;",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // address
                UnaryOpcode::Address as u8,
                // one bindings
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                ControlOpcode::GetGlobal as u8,
                0x00,
                0x00,
                // dereference
                UnaryOpcode::Dereference as u8,
                ControlOpcode::Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            "let one = &1; let *one = 2;",
            vec![
                // 1
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // address
                UnaryOpcode::Address as u8,
                // one binding
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                // 2
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                // one pointer dereference
                ControlOpcode::SetPointer as u8,
                0x00,
                0x00,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "let num = 55; fn() { num }",
            vec![
                // 55
                ControlOpcode::Constant as u8,
                0x00,
                0x00,
                // num binding
                ControlOpcode::SetGlobal as u8,
                0x00,
                0x00,
                // fn
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(55),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // num binding
                        ControlOpcode::GetGlobal as u8,
                        0x00,
                        0x00,
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 0,
                }),
            ],
        ),
        (
            "
                fn() {
                    let num = 55;
                    num
                }
            ",
            vec![
                // fn
                ControlOpcode::Constant as u8,
                0x00,
                0x01,
                ControlOpcode::Pop as u8,
            ],
            vec![
                Object::Integer(55),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 55
                        ControlOpcode::Constant as u8,
                        0x00,
                        0x00,
                        // num bindings
                        ControlOpcode::SetLocal as u8,
                        0x00,
                        ControlOpcode::GetLocal as u8,
                        0x00,
                        ControlOpcode::ReturnValue as u8,
                    ],
                    num_locals: 1,
                }),
            ],
        ),
    ];

    for (input, instructions, constants) in &tests {
        let bc = compile(input);

        // Check constants first for easier debugging.
        assert_eq!(*constants, bc.constants, "unexpected constants");
        assert_instructions(&input, instructions, &bc.instructions);
    }
}

fn assert_instructions(input: &str, want: &[u8], got: &[u8]) {
    let want_ins = Instructions::parse(want).expect("failed to parse want instructions");
    let got_ins = Instructions::parse(got).expect("failed to parse got instructions");

    assert_eq!(
        want_ins, got_ins,
        "\ninput: {}, unexpected instructions stream:\nwant:\n{}\ngot:\n{}",
        input, want_ins, got_ins
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
