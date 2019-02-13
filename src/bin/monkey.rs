extern crate getopts;
extern crate mdl_monkey;

use mdl_monkey::ast;
use mdl_monkey::evaluator;
use mdl_monkey::lexer::Lexer;
use mdl_monkey::parser::Parser;
use mdl_monkey::token::Token;

use getopts::Options;
use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optflag("l", "lex", "display tokens produced by the lexer");

    let matches = opts.parse(&args[1..]).map_err(|err| err.to_string())?;

    // Present usage if '-h' or no arguments.
    if matches.opt_present("h") || matches.free.is_empty() {
        let brief = format!("Usage: {} 'let five = 5;'", program);
        print!("{}", opts.usage(&brief));
        return Ok(());
    }

    // Pass all free arguments to the lexer and parser.
    let input = matches.free.join(" ");

    if matches.opt_present("l") {
        lex(&input)?;
    }

    let prog = parse(&input)?;

    eval(ast::Node::Program(prog))?;

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

    println!();
    Ok(())
}

fn parse(input: &str) -> Result<ast::Program, String> {
    println!("parser:");

    let mut p = Parser::new(Lexer::new(input)).map_err(|err| err.to_string())?;

    let prog = p.parse().map_err(|err| err.to_string())?;
    for s in &prog.statements {
        println!("  - {}", s);
    }

    println!();
    Ok(prog)
}

fn eval(node: ast::Node) -> Result<(), String> {
    println!("eval:");

    let obj = evaluator::eval(node).map_err(|err| err.to_string())?;
    println!("  - {}", obj);

    Ok(())
}
