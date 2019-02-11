//! A parser for the Monkey programming language from <https://interpreterbook.com/>.

use crate::ast;
use crate::lexer::{self, Token};

use std::error;
use std::fmt;
use std::mem;
use std::rc::Rc;
use std::result;

/// Parses `Token`s and produces an `ast::Program` for the Monkey programming
/// language.
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,

    // Track the current and peek indices in the Tokens vector.
    current: Rc<Token>,
    peek: Rc<Token>,
}

impl<'a> Parser<'a> {
    /// Creates a new `Parser` by accepting an input vector of `Token`s.
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer,

            current: Rc::new(Token::Eof),
            peek: Rc::new(Token::Eof),
        };

        p.next_token();
        p.next_token();

        p
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
        // We're still in range; does tok match what we are searching for?
        Ok(*self.current == tok)
    }

    /// Peeks at the next `Token` and determines if it is the same type as `tok`.
    fn peek_is(&self, tok: Token) -> Result<bool> {
        // We're still in range; does tok match what we are searching for?
        Ok(*self.peek == tok)
    }

    /// Peeks at the next `Token` and expects it to be the same type as `tok`.
    fn expect_peek(&self, tok: Token) -> Result<()> {
        if self.peek_is(tok)? {
            Ok(())
        } else {
            Err(Error::UnexpectedToken(format!("{:?}", &self.peek)))
        }
    }

    /// Peeks and extracts the value from a `Token::Identifier`, or returns an
    /// error if the `Token` is of a different type.
    fn peek_extract_identifier(&self) -> Result<String> {
        if let Token::Identifier(ref id) = *self.peek {
            Ok(id.clone())
        } else {
            Err(Error::UnexpectedToken(format!("{:?}", self.peek)))
        }
    }

    /// Advances the parser once in its `Token`s vector.
    fn next_token(&mut self) {
        // current takes the value of peek, and peek is overwritten immediately
        // after by the next token.
        mem::swap(&mut self.current, &mut self.peek);
        self.peek = Rc::new(self.lexer.next_token());
    }

    /// Parses a let or return statement.
    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match *self.current {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),

            _ => Err(Error::UnexpectedToken(format!("{:?}", self.current))),
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
