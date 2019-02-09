extern crate mdl_monkey;

use mdl_monkey::lexer::{Lexer, Token};
use std::process;

fn main() -> Result<(), Box<std::error::Error>> {
    let mut l = Lexer::new("let five = 5;");

    for t in l.lex() {
        match t {
            Token::Eof => {
                break;
            }
            Token::Illegal(ill) => {
                println!("illegal token: \"{}\"", ill);
                process::exit(1);
            }
            _ => {
                println!("{:?}", t);
            }
        };
    }

    Ok(())
}
