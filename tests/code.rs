extern crate mdl_monkey;

use mdl_monkey::code::*;

use std::io;

#[test]
fn code_make_parse_ok() {
    let tests = vec![
        (
            Opcode::Control(ControlOpcode::Constant),
            vec![65534],
            vec![ControlOpcode::Constant as u8, 0xff, 0xfe],
        ),
        (
            Opcode::Control(ControlOpcode::Pop),
            vec![],
            vec![ControlOpcode::Pop as u8],
        ),
        (
            Opcode::Control(ControlOpcode::True),
            vec![],
            vec![ControlOpcode::True as u8],
        ),
        (
            Opcode::Control(ControlOpcode::False),
            vec![],
            vec![ControlOpcode::False as u8],
        ),
        (
            Opcode::Unary(UnaryOpcode::Negate),
            vec![],
            vec![UnaryOpcode::Negate as u8],
        ),
        (
            Opcode::Unary(UnaryOpcode::Not),
            vec![],
            vec![UnaryOpcode::Not as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::Add),
            vec![],
            vec![BinaryOpcode::Add as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::Sub),
            vec![],
            vec![BinaryOpcode::Sub as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::Mul),
            vec![],
            vec![BinaryOpcode::Mul as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::Div),
            vec![],
            vec![BinaryOpcode::Div as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::Mod),
            vec![],
            vec![BinaryOpcode::Mod as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::Equal),
            vec![],
            vec![BinaryOpcode::Equal as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::NotEqual),
            vec![],
            vec![BinaryOpcode::NotEqual as u8],
        ),
        (
            Opcode::Binary(BinaryOpcode::GreaterThan),
            vec![],
            vec![BinaryOpcode::GreaterThan as u8],
        ),
    ];

    for (op, operands, want) in &tests {
        let bc = make(*op, operands).expect("make returned an error");
        assert_eq!(*want, bc);

        // We only produce one instruction so only check the first in the stream.
        let ins = Instructions::parse(&bc).expect("parse returned an error");
        assert_eq!(*op, ins.stream[0].0);
        assert_eq!(*operands, ins.stream[0].1);
    }
}

#[test]
fn code_make_error() {
    let tests = vec![
        (
            Opcode::Control(ControlOpcode::Constant),
            vec![],
            ErrorKind::BadNumberOperands { want: 1, got: 0 },
        ),
        (
            Opcode::Control(ControlOpcode::Pop),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Control(ControlOpcode::True),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Control(ControlOpcode::False),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Unary(UnaryOpcode::Negate),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Unary(UnaryOpcode::Not),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::Add),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::Sub),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::Mul),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::Div),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::Mod),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::Equal),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::NotEqual),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Binary(BinaryOpcode::GreaterThan),
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
    ];

    for (op, operands, want) in &tests {
        let got = match make(*op, operands) {
            Ok(_) => panic!("make did not return an error"),
            Err(err) => match err {
                Error::Internal { kind, .. } => kind,
                _ => panic!("unhandled error: {:?}", err),
            },
        };

        assert_eq!(*want, got);
    }
}

#[test]
fn code_instructions_parse_io_unexpected_eof_error() {
    let tests = vec![vec![0x00, 0xff], vec![0x00, 0x00, 0x00, 0x00]];

    for tt in &tests {
        let kind = match Instructions::parse(tt) {
            Ok(_) => panic!("parse did not return an error"),
            Err(err) => match err {
                Error::Io(err) => err.kind(),
                _ => panic!("unhandled error: {:?}", err),
            },
        };

        assert_eq!(io::ErrorKind::UnexpectedEof, kind);
    }
}
