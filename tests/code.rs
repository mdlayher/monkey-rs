extern crate mdl_monkey;

use mdl_monkey::code::*;

use std::io;

#[test]
fn code_make_parse_ok() {
    let tests = vec![
        (
            Opcode::Constant,
            vec![65534],
            vec![Opcode::Constant as u8, 0xff, 0xfe],
        ),
        (Opcode::Add, vec![], vec![Opcode::Add as u8]),
        (Opcode::Pop, vec![], vec![Opcode::Pop as u8]),
        (Opcode::True, vec![], vec![Opcode::True as u8]),
        (Opcode::False, vec![], vec![Opcode::False as u8]),
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
            Opcode::Constant,
            vec![],
            ErrorKind::BadNumberOperands { want: 1, got: 0 },
        ),
        (
            Opcode::Pop,
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::Add,
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::True,
            vec![1],
            ErrorKind::BadNumberOperands { want: 0, got: 1 },
        ),
        (
            Opcode::False,
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
