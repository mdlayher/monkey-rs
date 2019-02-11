//! A parser for the Monkey programming language from <https://interpreterbook.com/>.

use crate::ast;
use crate::lexer::Token;

use std::error;
use std::fmt;
use std::result;

/// Parses `Token`s and produces an `ast::Program` for the Monkey programming
/// language.
pub struct Parser {
    tokens: Vec<Token>,

    // Track the current and peek indices in the Tokens vector.
    current: usize,
    peek: usize,
}

impl Parser {
    /// Creates a new `Parser` by accepting an input vector of `Token`s.
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,

            current: 0,
            peek: 1,
        }
    }

    /// Parses the input `Token` stream and creates an `ast::Program`.
    pub fn parse(&mut self) -> Result<ast::Program> {
        let mut prog = ast::Program::new();

        while !self.current_is(Token::Eof)? {
            let stmt = self.parse_statement()?;
            prog.statements.push(stmt);

            self.next_token();
        }

        Ok(prog)
    }

    /// Looks at the current `Token` and determines if it is the same type as `tok`.
    fn current_is(&self, tok: Token) -> Result<bool> {
        if tok != Token::Eof && self.current >= self.tokens.len() {
            // We weren't looking for an EOF, but we ended up out of range
            // anyway.
            Err(Error::UnexpectedEof)
        } else {
            // We're still in range; does tok match what we are searching for?
            Ok(self.tokens[self.current] == tok)
        }
    }

    /// Peeks at the next `Token` and determines if it is the same type as `tok`.
    fn peek_is(&self, tok: Token) -> Result<bool> {
        if self.peek >= self.tokens.len() {
            // We peeked past the end of the tokens vector.
            Err(Error::UnexpectedEof)
        } else {
            // We're still in range; does tok match what we are searching for?
            Ok(self.tokens[self.peek] == tok)
        }
    }

    /// Peeks at the next `Token` and expects it to be the same type as `tok`.
    fn expect_peek(&self, tok: Token) -> Result<()> {
        if self.peek_is(tok)? {
            Ok(())
        } else {
            Err(Error::UnexpectedToken(format!(
                "{:?}",
                self.tokens[self.peek]
            )))
        }
    }

    /// Peeks and extracts the value from a `Token::Identifier`, or returns an
    /// error if the `Token` is of a different type.
    fn peek_extract_identifier(&self) -> Result<String> {
        let t = &self.tokens[self.peek];
        match t {
            Token::Identifier(id) => Ok(id.clone()),
            _ => Err(Error::UnexpectedToken(format!("{:?}", t))),
        }
    }

    /// Advances the parser once in its `Token`s vector.
    fn next_token(&mut self) {
        self.current = self.peek;
        self.peek += 1;
    }

    /// Parses a let or return statement.
    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match &self.tokens[self.current] {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),

            _ => Err(Error::UnexpectedToken(format!(
                "{:?}",
                self.tokens[self.current]
            ))),
        }
    }

    /// Parses a let statement.
    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        let name = self.peek_extract_identifier()?;
        self.next_token();

        self.expect_peek(Token::Assign)?;
        self.next_token();

        while !self.current_is(Token::Semicolon)? {
            self.next_token();
        }

        Ok(ast::Statement::Let(ast::LetStatement {
            name: ast::Identifier {
                value: name.to_string(),
            },
            value: ast::Expression::Todo,
        }))
    }

    /// Parses a return statement.
    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();

        while !self.current_is(Token::Semicolon)? {
            self.next_token();
        }

        Ok(ast::Statement::Return(ast::ReturnStatement {
            value: ast::Expression::Todo,
        }))
    }
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    UnexpectedToken(String),
    UnexpectedEof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UnexpectedToken(ref s) => write!(f, "parser found unexpected token: {}", s),
            Error::UnexpectedEof => write!(f, "parser found unexpected EOF"),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
