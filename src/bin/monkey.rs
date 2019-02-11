extern crate getopts;
extern crate mdl_monkey;

use getopts::Options;
use mdl_monkey::lexer::{Lexer, Token};
use mdl_monkey::parser::Parser;
use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optflag("l", "lex", "only perform the lexing process");

    let matches = opts.parse(&args[1..]).map_err(|err| err.to_string())?;

    // Present usage if '-h' or no arguments.
    if matches.opt_present("h") || matches.free.is_empty() {
        let brief = format!("Usage: {} 'let five = 5;'", program);
        print!("{}", opts.usage(&brief));
        return Ok(());
    }

    // Pass all free arguments to the lexer and parser.
    let program = matches.free.join(" ");

    lex(&program)?;

    if matches.opt_present("l") {
        return Ok(());
    }

    parse(&program)?;

    Ok(())
}

fn lex(input: &str) -> Result<(), String> {
    println!("lexer:");

    let mut l = Lexer::new(input);

    let tokens = l.lex().map_err(|err| err.to_string())?;

    for t in &tokens {
        match t {
            Token::Eof => {
                break;
            }
            Token::Illegal(ill) => {
                return Err(format!("illegal token: {}", ill));
            }
            _ => {
                println!("  - {:?}", t);
            }
        };
    }

    Ok(())
}

fn parse(input: &str) -> Result<(), String> {
    println!("\nparser:");

    let mut p = Parser::new(Lexer::new(input)).map_err(|err| err.to_string())?;

    let prog = p.parse().map_err(|err| err.to_string())?;
    for s in prog.statements {
        println!("  - {}", s);
    }

    Ok(())
}
