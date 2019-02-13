//! A parser for the Monkey programming language from <https://interpreterbook.com/>.

use crate::token::Token;
use crate::{ast, lexer};

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
                want: format!("{}", tok),
                got: format!("{}", &self.current),
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
            if let Token::Identifier(id) = &self.current {
                // If so, return its name.
                Ok(id.to_string())
            } else {
                Err(Error::UnexpectedToken {
                    want: "identifier".to_string(),
                    got: format!("{}", &self.current),
                })
            }
        }?;

        self.expect(Token::Assign)?;

        while self.current != Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Let(ast::LetStatement {
            name,
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
    fn parse_expression(&mut self, prec: Precedence) -> Result<ast::Expression> {
        let mut left = self.prefix_parse()?;

        // Continue until we reach a semicolon or a lower precedence operator.
        let prec_val = prec as u32;
        while self.peek != Token::Semicolon && prec_val < (precedence(&self.peek) as u32) {
            match self.infix_parse(&left) {
                Some(infix) => left = infix?,
                None => {
                    return Ok(left);
                }
            };
        }

        Ok(left)
    }

    /// Dispatches the appropriate function to deal with a prefix operator, if
    /// applicable.
    fn prefix_parse(&mut self) -> Result<ast::Expression> {
        match self.current {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Integer { .. } => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => self.parse_boolean_literal(),
            Token::LeftParen => self.parse_grouped_expression(),

            // TODO(mdlayher): better error for this.
            _ => Err(Error::UnexpectedToken {
                want: "matching prefix parse function".to_string(),
                got: format!("{}", self.current),
            }),
        }
    }

    /// Dispatches the appropriate function to deal with an infix operator, if
    /// applicable.
    fn infix_parse(&mut self, left: &ast::Expression) -> Option<Result<ast::Expression>> {
        match self.peek {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Percent
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => Some(self.parse_infix_expression(left)),

            // No infix parsing function.
            _ => None,
        }
    }

    /// Parses an identifier expression.
    fn parse_identifier(&self) -> Result<ast::Expression> {
        // Have we found an identifier for this expression?
        if let Token::Identifier(id) = &self.current {
            // If so, return its name.
            Ok(ast::Expression::Identifier(id.to_string()))
        } else {
            Err(Error::UnexpectedToken {
                want: "identifier".to_string(),
                got: format!("{}", &self.current),
            })
        }
    }

    /// Parses an integer literal expression.
    fn parse_integer_literal(&self) -> Result<ast::Expression> {
        // Have we found an integer for this expression?
        if let Token::Integer(int) = &self.current {
            // If so, return its value.
            Ok(ast::Expression::Integer(int.clone()))
        } else {
            Err(Error::UnexpectedToken {
                want: "integer".to_string(),
                got: format!("{}", &self.current),
            })
        }
    }

    /// Parses a boolean literal expression.
    fn parse_boolean_literal(&self) -> Result<ast::Expression> {
        match &self.current {
            Token::True => Ok(ast::Expression::Boolean(true)),
            Token::False => Ok(ast::Expression::Boolean(false)),
            _ => Err(Error::UnexpectedToken {
                want: "boolean".to_string(),
                got: format!("{}", &self.current),
            }),
        }
    }

    /// Parses a unary operator-prefixed expression.
    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let operator = self.current.clone();

        self.next_token()?;

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Ok(ast::Expression::Prefix(ast::PrefixExpression {
            operator,
            right,
        }))
    }

    /// Parses a binary operator infix expression.
    fn parse_infix_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        // Advance past left expression.
        self.next_token()?;

        // Capture operator and determine its precedence.
        let operator = self.current.clone();

        let prec = precedence(&self.current);
        self.next_token()?;

        // Parse right expression.
        let right = Box::new(self.parse_expression(prec)?);

        Ok(ast::Expression::Infix(ast::InfixExpression {
            left: Box::new(left.clone()),
            operator,
            right,
        }))
    }

    /// Parses an expression grouped inside parentheses.
    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        // Advance past left parenthesis.
        self.next_token()?;

        // Parse inner expression.
        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect(Token::RightParen)?;

        Ok(expr)
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

// Determines the Precedence value of a given Token.
fn precedence(tok: &Token) -> Precedence {
    match tok {
        Token::Equal => Precedence::Equals,
        Token::NotEqual => Precedence::Equals,
        Token::LessThan => Precedence::LessGreater,
        Token::GreaterThan => Precedence::LessGreater,
        Token::Plus => Precedence::Sum,
        Token::Minus => Precedence::Sum,
        Token::Slash => Precedence::Product,
        Token::Asterisk => Precedence::Product,
        Token::Percent => Precedence::Product,

        _ => Precedence::Lowest,
    }
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
        match self {
            Error::LexerError(err) => write!(f, "error from lexer: {}", err),
            Error::UnexpectedToken { want, got } => write!(
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
        match self {
            Error::LexerError(err) => Some(err),
            _ => None,
        }
    }
}
