extern crate mdl_monkey;

use mdl_monkey::lexer::{Error, Lexer};
use mdl_monkey::token::{Radix, Token};

#[test]
fn lex_next_token() {
    let got = Lexer::new(
        "
let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
1.01 2.
",
    )
    .lex()
    .expect("failed to lex tokens");

    let want = vec![
        //
        Token::Let,
        Token::Identifier("five".to_string()),
        Token::Assign,
        Token::Integer {
            radix: Radix::Decimal,
            value: 5,
        },
        Token::Semicolon,
        //
        Token::Let,
        Token::Identifier("ten".to_string()),
        Token::Assign,
        Token::Integer {
            radix: Radix::Decimal,
            value: 10,
        },
        Token::Semicolon,
        //
        Token::Let,
        Token::Identifier("add".to_string()),
        Token::Assign,
        Token::Function,
        Token::LeftParen,
        Token::Identifier("x".to_string()),
        Token::Comma,
        Token::Identifier("y".to_string()),
        Token::RightParen,
        Token::LeftBrace,
        Token::Identifier("x".to_string()),
        Token::Plus,
        Token::Identifier("y".to_string()),
        Token::Semicolon,
        Token::RightBrace,
        Token::Semicolon,
        //
        Token::Let,
        Token::Identifier("result".to_string()),
        Token::Assign,
        Token::Identifier("add".to_string()),
        Token::LeftParen,
        Token::Identifier("five".to_string()),
        Token::Comma,
        Token::Identifier("ten".to_string()),
        Token::RightParen,
        Token::Semicolon,
        //
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Integer {
            radix: Radix::Decimal,
            value: 5,
        },
        Token::Semicolon,
        //
        Token::Integer {
            radix: Radix::Decimal,
            value: 5,
        },
        Token::LessThan,
        Token::Integer {
            radix: Radix::Decimal,
            value: 10,
        },
        Token::GreaterThan,
        Token::Integer {
            radix: Radix::Decimal,
            value: 5,
        },
        Token::Semicolon,
        //
        Token::If,
        Token::LeftParen,
        Token::Integer {
            radix: Radix::Decimal,
            value: 5,
        },
        Token::LessThan,
        Token::Integer {
            radix: Radix::Decimal,
            value: 10,
        },
        Token::RightParen,
        Token::LeftBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RightBrace,
        Token::Else,
        Token::LeftBrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RightBrace,
        //
        Token::Integer {
            radix: Radix::Decimal,
            value: 10,
        },
        Token::Equal,
        Token::Integer {
            radix: Radix::Decimal,
            value: 10,
        },
        Token::Semicolon,
        //
        Token::Integer {
            radix: Radix::Decimal,
            value: 10,
        },
        Token::NotEqual,
        Token::Integer {
            radix: Radix::Decimal,
            value: 9,
        },
        Token::Semicolon,
        //
        Token::Float(1.01),
        Token::Float(2.),
        //
        Token::Eof,
    ];

    assert_tokens_equal(&want, &got);
}

#[test]
fn lex_integer_literals() {
    let got = Lexer::new(
        "
101
0b101
0101
0o101
0x101;
",
    )
    .lex()
    .expect("failed to lex tokens");

    let want = vec![
        Token::Integer {
            radix: Radix::Decimal,
            value: 101,
        },
        Token::Integer {
            radix: Radix::Binary,
            value: 0b101,
        },
        Token::Integer {
            radix: Radix::Octal,
            // Rust doesn't support C-style octal literals.
            value: 0o101,
        },
        Token::Integer {
            radix: Radix::Octal,
            value: 0o101,
        },
        Token::Integer {
            radix: Radix::Hexadecimal,
            value: 0x101,
        },
        Token::Semicolon,
        Token::Eof,
    ];

    assert_tokens_equal(&want, &got);
}

#[test]
fn lex_illegal_number() {
    let inputs = vec!["0bX", "0xX", "0oX", "1X"];

    for input in inputs {
        let err = Lexer::new(input)
            .lex()
            .expect_err(&format!("expected illegal number error for {}", input));

        match err {
            Error::IllegalInteger(_) => {}
            _ => panic!("unexpected error type: {:?}", err),
        };
    }
}

#[test]
fn lex_illegal_number_radix() {
    let err = Lexer::new("0q000")
        .lex()
        .expect_err("expected illegal radix error");

    assert_eq!(err, Error::IllegalIntegerRadix('q'));
}

fn assert_tokens_equal(want: &[Token], got: &[Token]) {
    assert_eq!(want.len(), got.len());

    for (a, b) in want.iter().zip(got) {
        assert_eq!(*a, *b);
    }
}
