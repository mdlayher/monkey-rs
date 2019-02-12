//! A parser for the Monkey programming language from <https://interpreterbook.com/>.

use crate::ast;
use crate::lexer::{self, Token};

use std::error;
use std::fmt;
use std::mem;
use std::result;

/// Consumes input from a `lexer::Lexer` and produces an `ast::Program` for the
/// Monkey programming language.
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,

    // Track the current and peek tokens from the Lexer.
    current: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    /// Creates a new `Parser` by accepting a `lexer::Lexer`.
    pub fn new(lexer: lexer::Lexer<'a>) -> Result<Self> {
        let mut p = Parser {
            lexer,

            current: Token::Eof,
            peek: Token::Eof,
        };

        p.next_token()?;
        p.next_token()?;

        Ok(p)
    }

    /// Parses the input program by consuming tokens from the `lexer::Lexer`,
    /// creating an `ast::Program`.
    pub fn parse(&mut self) -> Result<ast::Program> {
        let mut prog = ast::Program::new();

        while self.current != Token::Eof {
            let stmt = self.parse_statement()?;
            prog.statements.push(stmt);

            self.next_token()?;
        }

        Ok(prog)
    }

    /// Consumes the next `Token` and expects it to be the same type as `tok`.
    fn expect(&mut self, tok: Token) -> Result<()> {
        self.next_token()?;

        if self.current == tok {
            Ok(())
        } else {
            Err(Error::UnexpectedToken {
                want: format!("{:?}", tok),
                got: format!("{:?}", &self.current),
            })
        }
    }

    /// Advances the parser once in its `Token`s vector.
    fn next_token(&mut self) -> Result<()> {
        // current takes the value of peek, and peek is overwritten immediately
        // after by the next token.
        mem::swap(&mut self.current, &mut self.peek);
        self.peek = self.lexer.next_token().map_err(Error::LexerError)?;

        Ok(())
    }

    /// Parses a let, return, or expression statement..
    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.current {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),

            // Anything else must be an expression.
            _ => self.parse_expression_statement(),
        }
    }

    /// Parses a let statement.
    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;
        let name = {
            // Have we found an identifier for the Let statement?
            if let Token::Identifier(ref id) = self.current {
                // If so, return its name.
                Ok(id.to_string())
            } else {
                Err(Error::UnexpectedToken {
                    want: "identifier".to_string(),
                    got: format!("{:?}", &self.current),
                })
            }
        }?;

        self.expect(Token::Assign)?;

        while self.current != Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Let(ast::LetStatement {
            name: ast::Identifier { value: name },
            value: ast::Expression::Todo,
        }))
    }

    /// Parses a return statement.
    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        while self.current != Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Return(ast::ReturnStatement {
            value: ast::Expression::Todo,
        }))
    }

    /// Parses an expression statement.
    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Expression(expr))
    }

    /// Parses an expression.
    fn parse_expression(&mut self, _prec: Precedence) -> Result<ast::Expression> {
        self.prefix_parse()
    }

    /// Dispatches the appropriate function to deal with a prefix operator, if
    /// applicable.
    fn prefix_parse(&mut self) -> Result<ast::Expression> {
        match self.current {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Integer { .. } => self.parse_integer_literal(),
            _ => Err(Error::UnexpectedToken {
                want: "matching prefix parse function".to_string(),
                got: format!("{:?}", self.current),
            }),
        }
    }

    /// Parses an identifier expression.
    fn parse_identifier(&mut self) -> Result<ast::Expression> {
        // Have we found an identifier for this expression?
        if let Token::Identifier(ref id) = self.current {
            // If so, return its name.
            Ok(ast::Expression::Identifier(ast::Identifier {
                value: id.to_string(),
            }))
        } else {
            Err(Error::UnexpectedToken {
                want: "identifier".to_string(),
                got: format!("{:?}", &self.current),
            })
        }
    }

    /// Parses an integer literal expression.
    fn parse_integer_literal(&mut self) -> Result<ast::Expression> {
        // Have we found an integer for this expression?
        if let Token::Integer { value: int, .. } = self.current {
            // If so, return its value
            Ok(ast::Expression::Integer(int))
        } else {
            Err(Error::UnexpectedToken {
                want: "integer".to_string(),
                got: format!("{:?}", &self.current),
            })
        }
    }
}

/// Denotes the precedence of various operators.
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    LexerError(lexer::Error),
    UnexpectedToken { want: String, got: String },
    UnexpectedEof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::LexerError(ref err) => write!(f, "error from lexer: {}", err),
            Error::UnexpectedToken { ref want, ref got } => write!(
                f,
                "parser found unexpected token: {}, expected: {}",
                got, want,
            ),
            Error::UnexpectedEof => write!(f, "parser found unexpected EOF"),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::LexerError(ref err) => Some(err),
            _ => None,
        }
    }
}
