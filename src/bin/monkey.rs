extern crate getopts;
extern crate mdl_monkey;

use getopts::Options;
use mdl_monkey::lexer::{Lexer, Token};
use std::env;
use std::error;

fn main() -> Result<(), Box<error::Error>> {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(&args[1..])?;

    // Present usage if '-h' or no arguments.
    if matches.opt_present("h") || matches.free.is_empty() {
        let brief = format!("Usage: {} 'let five = 5;'", program);
        print!("{}", opts.usage(&brief));
        return Ok(());
    }

    // Pass all free arguments to the lexer.
    lex(&matches.free.join(" "))?;

    Ok(())
}

fn lex(input: &str) -> Result<(), String> {
    let mut l = Lexer::new(input);

    for t in l.lex() {
        match t {
            Token::Eof => {
                break;
            }
            Token::Illegal(ill) => {
                return Err(format!("illegal token: {}", ill));
            }
            _ => {
                println!("{:?}", t);
            }
        };
    }

    Ok(())
}
