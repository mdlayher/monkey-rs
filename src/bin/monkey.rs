extern crate getopts;
extern crate mdl_monkey;

use getopts::Options;
use mdl_monkey::lexer::{Lexer, Token};
use mdl_monkey::parser::Parser;
use std::env;
use std::error;
use std::process;

fn main() -> Result<(), Box<error::Error>> {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optflag("l", "lex", "only perform the lexing process");

    let matches = opts.parse(&args[1..])?;

    // Present usage if '-h' or no arguments.
    if matches.opt_present("h") || matches.free.is_empty() {
        let brief = format!("Usage: {} 'let five = 5;'", program);
        print!("{}", opts.usage(&brief));
        return Ok(());
    }

    // Pass all free arguments to the lexer and parser.
    let program = matches.free.join(" ");

    if let Err(err) = lex(&program) {
        println!("\nlexer error: {}", err);
        process::exit(1);
    };

    if matches.opt_present("l") {
        return Ok(());
    }

    if let Err(err) = parse(&program) {
        println!("\nparser error: {}", err);
        process::exit(1);
    };

    Ok(())
}

fn lex(input: &str) -> Result<(), String> {
    println!("lexer:");

    let mut l = Lexer::new(input);

    let tokens = l.lex();
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

    let mut p = Parser::new(Lexer::new(input));

    let prog = match p.parse() {
        Ok(prog) => prog,
        Err(err) => {
            return Err(err.to_string());
        }
    };

    for s in prog.statements {
        println!("  - {}", s);
    }

    Ok(())
}
