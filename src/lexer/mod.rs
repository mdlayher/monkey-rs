//! A lexer for the Monkey programming language from <https://interpreterbook.com/>.

/// The types of tokens recognized by a `Lexer`, along with their associated
/// data if applicable.
#[derive(Debug, PartialEq)]
pub enum Token {
    // Control tokens.
    Illegal(char),
    Eof,

    // Identifiers and literals.
    Identifier(String),
    Integer(String),

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
            input: input,
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
    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        // Consume tokens from the stream until Eof.
        loop {
            let t = self.next_token();
            match t {
                Token::Eof => {
                    tokens.push(t);
                    return tokens;
                }
                _ => {
                    tokens.push(t);
                }
            }
        }
    }

    /// Advances the lexer once and produces a single Token.
    pub fn next_token(&mut self) -> Token {
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
                        return key;
                    } else {
                        return Token::Identifier(ident);
                    }
                } else if self.ch.is_numeric() {
                    return Token::Integer(self.read_number());
                } else {
                    // No known tokens for this character, return Illegal.
                    Token::Illegal(self.ch)
                }
            }
        };

        // Advance to the next character in preparation for the next call.
        self.read_char();
        t
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

    // Reads a number from a string.
    fn read_number(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_numeric() {
            self.read_char();
        }

        self.input
            .chars()
            .skip(pos)
            .take(self.position - pos)
            .collect()
    }

    // Advances the lexer until all contiguous whitespace is consumed.
    fn skip_whitespace(&mut self) -> () {
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
    return c.is_ascii_alphabetic() || c == '_';
}
