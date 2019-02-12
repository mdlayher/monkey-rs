//! A lexer for the Monkey programming language from <https://interpreterbook.com/>.

use crate::token::{Radix, Token};

use std::error;
use std::fmt;
use std::num;
use std::result;
use std::str::FromStr;

/// Lexes input and produces a stream of `Token`s for the Monkey programming
/// language.
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` by accepting an input string.
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0 as char,
        };

        // Advance once to ready the Lexer.
        l.read_char();
        l
    }

    /// Lexes all tokens from an input string and produces a vector of `Token`s
    /// until an `Eof` token is encountered.
    pub fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];

        // Consume tokens from the stream until Eof.
        loop {
            let t = self.next_token()?;
            match t {
                Token::Eof => {
                    tokens.push(t);
                    return Ok(tokens);
                }
                _ => {
                    tokens.push(t);
                }
            }
        }
    }

    /// Advances the lexer once and produces a single Token.
    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let t = match self.ch {
            '=' => {
                // Is this '==' or just '='?
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                // Is this '!=' or just '!'?
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '\u{0000}' => Token::Eof,

            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();

                    // Determine if this identifier is actually a keyword, and
                    // return that keyword if so.
                    if let Some(key) = lookup_keyword(&ident) {
                        return Ok(key);
                    } else {
                        return Ok(Token::Identifier(ident));
                    }
                } else if is_number(self.ch) {
                    // TODO(mdlayher): negative numbers.
                    return Ok(self.read_number()?);
                } else {
                    // No known tokens for this character, return Illegal.
                    Token::Illegal(self.ch)
                }
            }
        };

        // Advance to the next character in preparation for the next call.
        self.read_char();
        Ok(t)
    }

    // Peeks at the next character in the input without advancing the Lexer.
    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
        } else {
            // TODO(mdlayher): consider handling unicode?
            if let Some(ch) = self.input.chars().nth(self.read_position) {
                ch
            } else {
                panic!("peeked out of range character")
            }
        }
    }

    // Consumes the next character of input while advancing the Lexer.
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 as char;
        } else {
            // TODO(mdlayher): consider handling unicode?
            if let Some(ch) = self.input.chars().nth(self.read_position) {
                self.ch = ch;
            } else {
                panic!("read out of range character");
            }
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    // Reads an identifier or keyword string.
    fn read_identifier(&mut self) -> String {
        let pos = self.position;

        // Numbers okay in identifiers after first character.
        while is_letter(self.ch) || self.ch.is_numeric() {
            self.read_char();
        }

        self.input
            .chars()
            .skip(pos)
            .take(self.position - pos)
            .collect()
    }

    // Reads an integer or floating point number from a string.
    fn read_number(&mut self) -> Result<Token> {
        let pos = self.position;

        // Consume consecutive alphanumeric or period characters so it is
        // possible to parse integers with various radixes, as well as floating
        // point numbers.
        while (self.ch.is_ascii_alphanumeric() || self.ch == '.') && !self.ch.is_whitespace() {
            self.read_char();
        }

        let chars: Vec<char> = self
            .input
            .chars()
            .skip(pos)
            .take(self.position - pos)
            .collect();

        // TODO(mdlayher): this detection logic needs work.
        if chars.contains(&'.') {
            Ok(Token::Float(
                f64::from_str(&chars.iter().collect::<String>()).map_err(Error::IllegalFloat)?,
            ))
        } else {
            let token = parse_int(&chars)?;
            match token {
                Token::Integer { .. } => Ok(token),
                _ => {
                    panic!("parse_int returned a non-Token::Integer variant");
                }
            }
        }
    }

    // Advances the lexer until all contiguous whitespace is consumed.
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

// Produces Some(Token) if s matches a keyword, or None if not.
fn lookup_keyword(s: &str) -> Option<Token> {
    match s {
        "fn" => Some(Token::Function),
        "let" => Some(Token::Let),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "return" => Some(Token::Return),
        _ => None,
    }
}

// Determines if a character is considered a letter in Monkey.
fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

// Determines if a character is considered a number in Monkey.
fn is_number(c: char) -> bool {
    c >= '0' && c <= '9'
}

// Parses a Token::Integer from a sequence of characters.
fn parse_int(chars: &[char]) -> Result<Token> {
    // If the numeric string is too short to contain a radix, assume base 10.
    if chars.len() < 2 {
        let raw: String = chars.iter().collect();
        return Ok(Token::Integer {
            radix: Radix::Decimal,
            value: i64::from_str_radix(&raw, 10).map_err(Error::IllegalInteger)?,
        });
    }

    // Infer the radix and the number of prefix characters to skip when
    // parsing the numeric string.
    let (radix, skip) = match &chars[0..2] {
        // Binary literal.
        ['0', 'b'] => (Radix::Binary, 2),
        // Hexadecimal literal.
        ['0', 'x'] => (Radix::Hexadecimal, 2),
        // Octal literal.
        ['0', 'o'] => (Radix::Octal, 2),
        // C-style octal literal.
        ['0', '0'...'9'] => (Radix::Octal, 1),
        // Unknown radix prefix.
        ['0', r] => {
            return Err(Error::IllegalIntegerRadix(*r));
        }
        // Decimal literal.
        _ => (Radix::Decimal, 0),
    };

    let raw: String = chars.iter().skip(skip).collect();
    let base = match radix {
        Radix::Binary => 2,
        Radix::Decimal => 10,
        Radix::Hexadecimal => 16,
        Radix::Octal => 8,
    };

    Ok(Token::Integer {
        radix,
        value: i64::from_str_radix(&raw, base).map_err(Error::IllegalInteger)?,
    })
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug, PartialEq)]
pub enum Error {
    IllegalFloat(num::ParseFloatError),
    IllegalIntegerRadix(char),
    IllegalInteger(num::ParseIntError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::IllegalFloat(ref err) => write!(f, "illegal floating point number: {}", err),
            Error::IllegalIntegerRadix(ref r) => write!(f, "illegal number radix: {}", r),
            Error::IllegalInteger(ref err) => write!(f, "illegal integer number: {}", err),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::IllegalFloat(ref err) => Some(err),
            Error::IllegalInteger(ref err) => Some(err),
            _ => None,
        }
    }
}
