extern crate mdl_monkey;

use mdl_monkey::{
    ast,
    code::{BinaryOpcode::*, CompositeOpcode::*, ControlOpcode::*, Instructions, UnaryOpcode::*},
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
                Constant as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // +, pop
                Add as u8,
                Pop as u8,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "2; 4;",
            vec![
                // 2
                Constant as u8,
                0x00,
                0x00,
                Pop as u8,
                // 4
                Constant as u8,
                0x00,
                0x01,
                Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "1 * 1.0",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // 1.0
                Constant as u8,
                0x00,
                0x01,
                // *, pop
                Mul as u8,
                Pop as u8,
            ],
            vec![Object::Integer(1), Object::Float(1.0)],
        ),
        (
            "true; false;",
            vec![
                // true
                True as u8,
                Pop as u8,
                // false
                False as u8,
                Pop as u8,
            ],
            vec![],
        ),
        (
            "2 == 4",
            vec![
                // 2
                Constant as u8,
                0x00,
                0x00,
                // 4
                Constant as u8,
                0x00,
                0x01,
                // equal
                Equal as u8,
                Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "2 != 4",
            vec![
                // 2
                Constant as u8,
                0x00,
                0x00,
                // 4
                Constant as u8,
                0x00,
                0x01,
                // not equal
                NotEqual as u8,
                Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "2 > 4",
            vec![
                // 2
                Constant as u8,
                0x00,
                0x00,
                // 4
                Constant as u8,
                0x00,
                0x01,
                // greater than
                GreaterThan as u8,
                Pop as u8,
            ],
            vec![Object::Integer(2), Object::Integer(4)],
        ),
        (
            "2 < 4",
            // Compiler reorders the less-than to a greater-than operation.
            vec![
                // 4
                Constant as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // greater than
                GreaterThan as u8,
                Pop as u8,
            ],
            vec![Object::Integer(4), Object::Integer(2)],
        ),
        (
            "-1; -2;",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // negate
                Negate as u8,
                Pop as u8,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // negate
                Negate as u8,
                Pop as u8,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "!true; !!false;",
            vec![
                // true
                True as u8,
                // not
                Not as u8,
                Pop as u8,
                // false
                False as u8,
                // not, not
                Not as u8,
                Not as u8,
                Pop as u8,
            ],
            vec![],
        ),
        (
            "if (true) { 10 }; 3333;",
            vec![
                // true
                True as u8,
                // jump not true
                JumpNotTrue as u8,
                0x00,
                0x0a,
                // 10
                Constant as u8,
                0x00,
                0x00,
                // jump
                Jump as u8,
                0x00,
                0x0b,
                // null
                Null as u8,
                Pop as u8,
                // 3333
                Constant as u8,
                0x00,
                0x01,
                Pop as u8,
            ],
            vec![Object::Integer(10), Object::Integer(3333)],
        ),
        (
            "if (true) { 10 } else { 20 }; 3333;",
            vec![
                // true
                True as u8,
                // jump not true
                JumpNotTrue as u8,
                0x00,
                0x0a,
                // 10
                Constant as u8,
                0x00,
                0x00,
                // jump
                Jump as u8,
                0x00,
                0x0d,
                // 20
                Constant as u8,
                0x00,
                0x01,
                Pop as u8,
                // 3333
                Constant as u8,
                0x00,
                0x02,
                Pop as u8,
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
                Constant as u8,
                0x00,
                0x00,
                // set 1
                SetGlobal as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // set 2
                SetGlobal as u8,
                0x00,
                0x01,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "let one = 1; one;",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // set 1
                SetGlobal as u8,
                0x00,
                0x00,
                // get 1
                GetGlobal as u8,
                0x00,
                0x00,
                Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            "let one = 1; let two = one; two;",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // set one
                SetGlobal as u8,
                0x00,
                0x00,
                // get one
                GetGlobal as u8,
                0x00,
                0x00,
                // set two
                SetGlobal as u8,
                0x00,
                0x01,
                // get two
                GetGlobal as u8,
                0x00,
                0x01,
                Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            r#""monkey""#,
            vec![
                // monkey
                Constant as u8,
                0x00,
                0x00,
                Pop as u8,
            ],
            vec![Object::String("monkey".to_string())],
        ),
        (
            r#""mon" + "key""#,
            vec![
                // mon
                Constant as u8,
                0x00,
                0x00,
                // key
                Constant as u8,
                0x00,
                0x01,
                // +
                Add as u8,
                Pop as u8,
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
                Array as u8,
                0x00,
                0x00,
                Pop as u8,
            ],
            vec![],
        ),
        (
            "[1, 2, 3]",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // 3
                Constant as u8,
                0x00,
                0x02,
                // array
                Array as u8,
                0x00,
                0x03,
                Pop as u8,
            ],
            vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
        ),
        (
            "[1 + 2, 3 - 4]",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // +
                Add as u8,
                // 3
                Constant as u8,
                0x00,
                0x02,
                // 4
                Constant as u8,
                0x00,
                0x03,
                // -
                Sub as u8,
                // array
                Array as u8,
                0x00,
                0x02,
                Pop as u8,
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
                Hash as u8, 0x00, 0x00, Pop as u8,
            ],
            vec![],
        ),
        (
            "{1: 2, 3: 4}",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // 3
                Constant as u8,
                0x00,
                0x02,
                // 4
                Constant as u8,
                0x00,
                0x03,
                // hash
                Hash as u8,
                0x00,
                0x04,
                Pop as u8,
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
                Array as u8,
                0x00,
                0x00,
                // 0
                Constant as u8,
                0x00,
                0x00,
                // index
                Index as u8,
                Pop as u8,
            ],
            vec![Object::Integer(0)],
        ),
        (
            r#"{}["foo"]"#,
            vec![
                // hash
                Hash as u8,
                0x00,
                0x00,
                // foo
                Constant as u8,
                0x00,
                0x00,
                // index
                Index as u8,
                Pop as u8,
            ],
            vec![Object::String("foo".to_string())],
        ),
        (
            "set{0, 1}",
            vec![
                // 0
                Constant as u8,
                0x00,
                0x00,
                // 1
                Constant as u8,
                0x00,
                0x01,
                // set
                Set as u8,
                0x00,
                0x02,
                Pop as u8,
            ],
            vec![Object::Integer(0), Object::Integer(1)],
        ),
        (
            "set{}[0]",
            vec![
                // set
                Set as u8,
                0x00,
                0x00,
                // 0
                Constant as u8,
                0x00,
                0x00,
                Index as u8,
                Pop as u8,
            ],
            vec![Object::Integer(0)],
        ),
        (
            "fn() { return 5 + 10 }",
            vec![
                // function
                Closure as u8,
                0x00,
                0x02,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 5
                        Constant as u8,
                        0x00,
                        0x00,
                        // 10
                        Constant as u8,
                        0x00,
                        0x01,
                        // add, return
                        Add as u8,
                        ReturnValue as u8,
                    ],
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
        ),
        (
            "fn() { 5 + 10 }",
            vec![
                // function
                Closure as u8,
                0x00,
                0x02,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 5
                        Constant as u8,
                        0x00,
                        0x00,
                        // 10
                        Constant as u8,
                        0x00,
                        0x01,
                        // add, return
                        Add as u8,
                        ReturnValue as u8,
                    ],
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
        ),
        (
            "fn() { 1; 2 }",
            vec![
                // function
                Closure as u8,
                0x00,
                0x02,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 1
                        Constant as u8,
                        0x00,
                        0x00,
                        Pop as u8,
                        // 2
                        Constant as u8,
                        0x00,
                        0x01,
                        ReturnValue as u8,
                    ],
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
        ),
        (
            "fn() { }",
            vec![
                // function
                Closure as u8,
                0x00,
                0x00,
                0x00,
                Pop as u8,
            ],
            vec![Object::CompiledFunction(object::CompiledFunction {
                instructions: vec![Return as u8],
                num_locals: 0,
                num_parameters: 0,
            })],
        ),
        (
            "fn() { 24 }();",
            vec![
                // function
                Closure as u8,
                0x00,
                0x01,
                0x00,
                // call
                Call as u8,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(24),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 24
                        Constant as u8,
                        0x00,
                        0x00,
                        // return
                        ReturnValue as u8,
                    ],
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
        ),
        (
            "let noArg = fn() { 24 }; noArg();",
            vec![
                // function
                Closure as u8,
                0x00,
                0x01,
                0x00,
                // noArg bindings
                SetGlobal as u8,
                0x00,
                0x00,
                GetGlobal as u8,
                0x00,
                0x00,
                // call
                Call as u8,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(24),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 24
                        Constant as u8,
                        0x00,
                        0x00,
                        // return
                        ReturnValue as u8,
                    ],
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
        ),
        (
            "let one = &1; *one;",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // address
                Address as u8,
                // one bindings
                SetGlobal as u8,
                0x00,
                0x00,
                GetGlobal as u8,
                0x00,
                0x00,
                // dereference
                Dereference as u8,
                Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            "let one = &1; let *one = 2;",
            vec![
                // 1
                Constant as u8,
                0x00,
                0x00,
                // address
                Address as u8,
                // one binding
                SetGlobal as u8,
                0x00,
                0x00,
                // 2
                Constant as u8,
                0x00,
                0x01,
                // one pointer dereference
                SetPointer as u8,
                0x00,
                0x00,
            ],
            vec![Object::Integer(1), Object::Integer(2)],
        ),
        (
            "let num = 55; fn() { num }",
            vec![
                // 55
                Constant as u8,
                0x00,
                0x00,
                // num binding
                SetGlobal as u8,
                0x00,
                0x00,
                // fn
                Closure as u8,
                0x00,
                0x01,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(55),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // num binding
                        GetGlobal as u8,
                        0x00,
                        0x00,
                        ReturnValue as u8,
                    ],
                    num_locals: 0,
                    num_parameters: 0,
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
                Closure as u8,
                0x00,
                0x01,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(55),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // 55
                        Constant as u8,
                        0x00,
                        0x00,
                        // num bindings
                        SetLocal as u8,
                        0x00,
                        GetLocal as u8,
                        0x00,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 0,
                }),
            ],
        ),
        (
            "
                let one = fn(a) { a };
                one(24);
            ",
            vec![
                // fn
                Closure as u8,
                0x00,
                0x00,
                0x00,
                // one bindings
                SetGlobal as u8,
                0x00,
                0x00,
                GetGlobal as u8,
                0x00,
                0x00,
                // 24
                Constant as u8,
                0x00,
                0x01,
                Call as u8,
                0x01,
                Pop as u8,
            ],
            vec![
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // a binding
                        GetLocal as u8,
                        0x00,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::Integer(24),
            ],
        ),
        (
            "
                let many = fn(a, b, c) { a; b; c };
                many(24, 25, 26);
            ",
            vec![
                // fn
                Closure as u8,
                0x00,
                0x00,
                0x00,
                // many bindings
                SetGlobal as u8,
                0x00,
                0x00,
                GetGlobal as u8,
                0x00,
                0x00,
                // 24
                Constant as u8,
                0x00,
                0x01,
                // 25
                Constant as u8,
                0x00,
                0x02,
                // 26
                Constant as u8,
                0x00,
                0x03,
                Call as u8,
                0x03,
                Pop as u8,
            ],
            vec![
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // a, b, c bindings
                        GetLocal as u8,
                        0x00,
                        Pop as u8,
                        GetLocal as u8,
                        0x01,
                        Pop as u8,
                        GetLocal as u8,
                        0x02,
                        ReturnValue as u8,
                    ],
                    num_locals: 3,
                    num_parameters: 3,
                }),
                Object::Integer(24),
                Object::Integer(25),
                Object::Integer(26),
            ],
        ),
        (
            "
                len([]);
                push([], 1);
            ",
            vec![
                // len
                GetBuiltin as u8,
                0x02,
                // array
                Array as u8,
                0x00,
                0x00,
                // call
                Call as u8,
                0x01,
                Pop as u8,
                // push
                GetBuiltin as u8,
                0x03,
                // array
                Array as u8,
                0x00,
                0x00,
                // 1
                Constant as u8,
                0x00,
                0x00,
                // call
                Call as u8,
                0x02,
                Pop as u8,
            ],
            vec![Object::Integer(1)],
        ),
        (
            "
                fn(a) {
                    fn(b) {
                        a + b
                    }
                }
            ",
            vec![Closure as u8, 0x00, 0x01, 0x00, Pop as u8],
            vec![
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // capture a
                        GetFree as u8,
                        0x00,
                        // get b
                        GetLocal as u8,
                        0x00,
                        // add both
                        Add as u8,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        // get a
                        GetLocal as u8,
                        0x00,
                        // func
                        Closure as u8,
                        0x00,
                        0x00,
                        0x01,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 1,
                }),
            ],
        ),
        (
            "
                let global = 55;
                fn() {
                    let a = 66;
                    fn() {
                        let b = 77;
                        fn() {
                            let c = 88;
                            global + a + b + c;
                        }
                    }
                }
            ",
            vec![
                Constant as u8,
                0x00,
                0x00,
                SetGlobal as u8,
                0x00,
                0x00,
                Closure as u8,
                0x00,
                0x06,
                0x00,
                Pop as u8,
            ],
            vec![
                Object::Integer(55),
                Object::Integer(66),
                Object::Integer(77),
                Object::Integer(88),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        Constant as u8,
                        0x00,
                        0x03,
                        SetLocal as u8,
                        0x00,
                        GetGlobal as u8,
                        0x00,
                        0x00,
                        GetFree as u8,
                        0x00,
                        Add as u8,
                        GetFree as u8,
                        0x01,
                        Add as u8,
                        GetLocal as u8,
                        0x00,
                        Add as u8,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 0,
                }),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        Constant as u8,
                        0x00,
                        0x02,
                        SetLocal as u8,
                        0x00,
                        GetFree as u8,
                        0x00,
                        GetLocal as u8,
                        0x00,
                        Closure as u8,
                        0x00,
                        0x04,
                        0x02,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 0,
                }),
                Object::CompiledFunction(object::CompiledFunction {
                    instructions: vec![
                        Constant as u8,
                        0x00,
                        0x01,
                        SetLocal as u8,
                        0x00,
                        GetLocal as u8,
                        0x00,
                        Closure as u8,
                        0x00,
                        0x05,
                        0x01,
                        ReturnValue as u8,
                    ],
                    num_locals: 1,
                    num_parameters: 0,
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
