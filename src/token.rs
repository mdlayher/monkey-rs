//! Syntax tokens for the Monkey programming language from
//! <https://interpreterbook.com/>.

use std::fmt;

/// The types of tokens recognized by a `Lexer`, along with their associated
/// data if applicable.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Control tokens.
    Illegal(char),
    Eof,

    // Identifiers and literals.
    Identifier(String),
    Integer(Integer),
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Illegal(c) => write!(f, "illegal({})", c),
            Token::Eof => write!(f, "EOF"),

            Token::Identifier(s) => write!(f, "identifier({})", s),
            Token::Integer(i) => i.fmt(f),
            Token::Float(fl) => fl.fmt(f),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),

            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),

            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}

/// An integer value and its associated radix.
#[derive(Clone, Debug, PartialEq)]
pub struct Integer {
    pub radix: Radix,
    pub value: i64,
}

/// The radix or base of an `Integer`.
#[derive(Clone, Debug, PartialEq)]
pub enum Radix {
    Binary,
    Decimal,
    Hexadecimal,
    Octal,
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.radix {
            Radix::Binary => write!(f, "0b{:b}", self.value),
            Radix::Decimal => write!(f, "{}", self.value),
            Radix::Hexadecimal => write!(f, "0x{:x}", self.value),
            Radix::Octal => write!(f, "0o{:o}", self.value),
        }
    }
}
