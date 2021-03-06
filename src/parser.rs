//! A parser for the Monkey programming language from <https://interpreterbook.com/>.

use crate::{ast, lexer, token::Token};

use std::{error, fmt, mem, result};

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

        // TODO(mdlayher): support multiple indirections.
        let is_pointer = self.current == Token::Asterisk;
        if is_pointer {
            self.next_token()?;
        }

        let name = self.parse_identifier_name()?;

        self.expect(Token::Assign)?;
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        if is_pointer {
            Ok(ast::Statement::LetDereference(
                ast::LetDereferenceStatement { name, value },
            ))
        } else {
            Ok(ast::Statement::Let(ast::LetStatement { name, value }))
        }
    }

    /// Parses a return statement.
    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Return(ast::ReturnStatement { value }))
    }

    /// Parses an expression statement.
    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Expression(expr))
    }

    /// Parses a block statement.
    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        let mut statements = vec![];

        // Keep consuming statements until end of block or EOF.
        while self.current != Token::RightBrace && self.current != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token()?;
        }

        Ok(ast::Statement::Block(ast::BlockStatement { statements }))
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
        match &self.current {
            Token::Identifier(_) => Ok(ast::Expression::Identifier(self.parse_identifier_name()?)),
            Token::Integer(i) => Ok(ast::Expression::Integer(*i)),
            Token::Float(f) => Ok(ast::Expression::Float(*f)),
            Token::String(s) => Ok(ast::Expression::String(s.to_string())),
            b @ Token::True | b @ Token::False => Ok(ast::Expression::Boolean(*b == Token::True)),
            Token::Bang | Token::Minus | Token::Ampersand | Token::Asterisk => {
                self.parse_prefix_expression()
            }
            Token::LeftParen => self.parse_grouped_expression(),
            Token::LeftBracket => {
                let elements = self.parse_expression_list(Token::RightBracket)?;
                Ok(ast::Expression::Array(ast::ArrayLiteral { elements }))
            }
            Token::LeftBrace => self.parse_hash_literal(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::Set => self.parse_set_literal(),

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
            Token::LeftParen => Some(self.parse_call_expression(left)),
            Token::LeftBracket => Some(self.parse_index_expression(left)),

            // No infix parsing function.
            _ => None,
        }
    }

    /// Parses an identifier's name.
    fn parse_identifier_name(&self) -> Result<String> {
        // Have we found an identifier for this expression?
        if let Token::Identifier(id) = &self.current {
            // If so, return its name.
            Ok(id.to_string())
        } else {
            Err(Error::UnexpectedToken {
                want: "identifier".to_string(),
                got: format!("{}", &self.current),
            })
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

    /// Parses an if/else expression.
    fn parse_if_expression(&mut self) -> Result<ast::Expression> {
        // Parse the opening of the if statement and conditional.
        self.expect(Token::LeftParen)?;
        self.next_token()?;

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect(Token::RightParen)?;
        self.expect(Token::LeftBrace)?;

        // Parse the body of the if block.
        let consequence = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(Error::UnexpectedToken {
                want: "if block statement".to_string(),
                got: format!("{}", &self.current),
            });
        };

        // Is there an associated else block with this if expression?
        if self.peek != Token::Else {
            return Ok(ast::Expression::If(ast::IfExpression {
                condition,
                consequence,
                alternative: None,
            }));
        }

        // Parse the body of the else block.
        self.next_token()?;
        self.expect(Token::LeftBrace)?;

        let alternative = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            Some(block)
        } else {
            return Err(Error::UnexpectedToken {
                want: "else block statement".to_string(),
                got: format!("{}", &self.current),
            });
        };

        Ok(ast::Expression::If(ast::IfExpression {
            condition,
            consequence,
            alternative,
        }))
    }

    /// Parses a function literal.
    fn parse_function_literal(&mut self) -> Result<ast::Expression> {
        self.expect(Token::LeftParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect(Token::LeftBrace)?;

        let body = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(Error::UnexpectedToken {
                want: "function body block statement".to_string(),
                got: format!("{}", &self.current),
            });
        };

        Ok(ast::Expression::Function(ast::FunctionLiteral {
            parameters,
            body,
        }))
    }

    /// Parses a list of function parameters.
    fn parse_function_parameters(&mut self) -> Result<Vec<String>> {
        let mut p = vec![];

        // End of parameter list?
        if self.peek == Token::RightParen {
            self.next_token()?;
            return Ok(p);
        }

        // Parse the first parameter.
        self.next_token()?;
        p.push(self.parse_identifier_name()?);

        // Parse each remaining parameter.
        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;

            p.push(self.parse_identifier_name()?);
        }

        self.expect(Token::RightParen)?;

        Ok(p)
    }

    /// Parses a function call infix expression.
    fn parse_call_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        self.next_token()?;

        // Are there zero arguments?
        if self.peek == Token::RightParen {
            self.next_token()?;
            return Ok(ast::Expression::Call(ast::CallExpression {
                function: Box::new(left.clone()),
                arguments: vec![],
            }));
        }

        let arguments = self.parse_expression_list(Token::RightParen)?;

        Ok(ast::Expression::Call(ast::CallExpression {
            function: Box::new(left.clone()),
            arguments,
        }))
    }

    /// Parses an array index infix expression.
    fn parse_index_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        self.next_token()?;
        self.next_token()?;

        let idx = ast::Expression::Index(ast::IndexExpression {
            left: Box::new(left.clone()),
            index: Box::new(self.parse_expression(Precedence::Lowest)?),
        });

        self.expect(Token::RightBracket)?;

        Ok(idx)
    }

    /// Parses a hash literal.
    fn parse_hash_literal(&mut self) -> Result<ast::Expression> {
        let mut pairs = vec![];
        while self.peek != Token::RightBrace {
            self.next_token()?;

            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect(Token::Colon)?;
            self.next_token()?;

            let value = self.parse_expression(Precedence::Lowest)?;

            // It's possible for duplicate keys to be parsed. This is handled
            // as a runtime error.
            pairs.push((key, value));

            // Any more items to parse?
            if self.peek != Token::RightBrace {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::RightBrace)?;

        Ok(ast::Expression::Hash(ast::HashLiteral { pairs }))
    }

    /// Parses a set literal.
    fn parse_set_literal(&mut self) -> Result<ast::Expression> {
        let mut set = vec![];
        self.expect(Token::LeftBrace)?;

        while self.peek != Token::RightBrace {
            self.next_token()?;

            // It's possible for duplicate elements to be parsed. This is
            // handled as a runtime error.
            set.push(self.parse_expression(Precedence::Lowest)?);

            // Any more items to parse?
            if self.peek != Token::RightBrace {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::RightBrace)?;
        Ok(ast::Expression::Set(ast::SetLiteral { set }))
    }

    /// Parses expressions until `end` is encountered.
    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<ast::Expression>> {
        // Special case: empty list.
        if self.peek == end {
            self.next_token()?;
            return Ok(vec![]);
        }

        let mut expressions = vec![];

        // Collect the remaining comma-separated expressions.
        self.next_token()?;
        expressions.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;

            expressions.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect(end)?;

        Ok(expressions)
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
    Index,
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
        Token::LeftParen => Precedence::Call,
        Token::LeftBracket => Precedence::Index,

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
    fn cause(&self) -> Option<&dyn error::Error> {
        match self {
            Error::LexerError(err) => Some(err),
            _ => None,
        }
    }
}
