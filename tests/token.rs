extern crate mdl_monkey;

use mdl_monkey::token::{Float, Integer, Radix, Token};

#[test]
fn token_display() {
    let tests = vec![
        (Token::Illegal('x'), "illegal(x)"),
        (Token::Eof, "EOF"),
        (
            Token::Identifier("string".to_string()),
            "identifier(string)",
        ),
        (
            Token::Integer(Integer {
                radix: Radix::Decimal,
                value: 101,
            }),
            "101",
        ),
        (
            Token::Integer(Integer {
                radix: Radix::Binary,
                value: 0b101,
            }),
            "0b101",
        ),
        (
            Token::Integer(Integer {
                radix: Radix::Octal,
                value: 0o101,
            }),
            "0o101",
        ),
        (
            Token::Integer(Integer {
                radix: Radix::Hexadecimal,
                value: 0x101,
            }),
            "0x101",
        ),
        (Token::Float(Float::from(1.23)), "1.23"),
        (Token::Assign, "="),
        (Token::Plus, "+"),
        (Token::Minus, "-"),
        (Token::Bang, "!"),
        (Token::Asterisk, "*"),
        (Token::Slash, "/"),
        (Token::Percent, "%"),
        (Token::Equal, "=="),
        (Token::NotEqual, "!="),
        (Token::LessThan, "<"),
        (Token::GreaterThan, ">"),
        (Token::Comma, ","),
        (Token::Colon, ":"),
        (Token::Semicolon, ";"),
        (Token::LeftParen, "("),
        (Token::RightParen, ")"),
        (Token::LeftBrace, "{"),
        (Token::RightBrace, "}"),
        (Token::Function, "fn"),
        (Token::Let, "let"),
        (Token::True, "true"),
        (Token::False, "false"),
        (Token::If, "if"),
        (Token::Else, "else"),
        (Token::Return, "return"),
    ];

    for test in tests {
        let (token, string) = test;

        assert_eq!(string, format!("{}", token));
    }
}
