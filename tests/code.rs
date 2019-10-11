extern crate mdl_monkey;

use mdl_monkey::code::*;

use std::io;

#[test]
fn code_make_parse_ok() {
    let tests = vec![(
        Opcode::Constant,
        vec![65534],
        vec![Opcode::Constant as u8, 0xff, 0xfe],
    )];

    for (op, operands, want) in &tests {
        let ins = make(*op, operands).expect("make returned an error");
        assert_eq!(*want, ins);

        let (got_op, got_operands) = parse(&ins).expect("parse returned an error");
        assert_eq!(*op, got_op);
        assert_eq!(*operands, got_operands);
    }
}

#[test]
fn code_make_error() {
    let tests = vec![(
        Opcode::Constant,
        vec![],
        ErrorKind::BadNumberOperands { want: 1, got: 0 },
    )];

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
fn code_parse_io_unexpected_eof_error() {
    let tests = vec![vec![], vec![0x00, 0xff]];

    for tt in &tests {
        let kind = match parse(tt) {
            Ok(_) => panic!("parse did not return an error"),
            Err(err) => match err {
                Error::Io(err) => err.kind(),
                _ => panic!("unhandled error: {:?}", err),
            },
        };

        assert_eq!(io::ErrorKind::UnexpectedEof, kind);
    }
}
