//! Syntax tokens for the Monkey programming language from
//! <https://interpreterbook.com/>.

/// The types of tokens recognized by a `Lexer`, along with their associated
/// data if applicable.
#[derive(Debug, PartialEq)]
pub enum Token {
    // Control tokens.
    Illegal(char),
    Eof,

    // Identifiers and literals.
    Identifier(String),
    Integer { radix: Radix, value: i64 },
    Float(f64),

    // Operators.
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    // Delimiters.
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Keywords.
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

/// The radix or base of a `Token::Integer`.
#[derive(Debug, PartialEq)]
pub enum Radix {
    Binary,
    Decimal,
    Hexadecimal,
    Octal,
}
