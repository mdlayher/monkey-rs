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
            "set{0, 0, 1}",
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
    ];

    for (input, instructions, constants) in &tests {
        let bc = compile(input);

        // Check constants first for easier debugging.
        assert_eq!(*constants, bc.constants, "unexpected constants");
        assert_instructions(instructions, &bc.instructions);
    }
}

#[test]
fn symbol_table_ok() {
    let mut st = SymbolTable::default();
    st.define("a".to_string());

    let tests = vec![
        (
            "b",
            Symbol {
                scope: Scope::Global,
                index: 1,
            },
        ),
        (
            "c",
            Symbol {
                scope: Scope::Global,
                index: 2,
            },
        ),
    ];

    for (name, symbol) in &tests {
        let idx = st.define(name.to_string());
        let s = st.resolve(name).expect("a symbol should be defined");

        assert_eq!(idx, s.index);
        assert_eq!(s, symbol);

        st.resolve(&"a").expect("a should always be defined");
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
