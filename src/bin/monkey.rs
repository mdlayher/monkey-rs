extern crate getopts;
extern crate mdl_monkey;

use mdl_monkey::{
    ast, compiler::Compiler, evaluator, lexer::Lexer, object::Environment, parser::Parser,
    token::Token, vm::Vm,
};

use getopts::Options;
use std::{env, time};

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag(
        "b",
        "bytecode",
        "display bytecode to be executed by the compiler/VM backend",
    );
    opts.optflag(
        "e",
        "eval",
        "use the eval interpreter backend instead of the compiler/VM backend",
    );
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

    if matches.opt_present("e") {
        eval(ast::Node::Program(prog))?;
    } else {
        run_vm(ast::Node::Program(prog), matches.opt_present("b"))?;
    }

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

    // TODO(mdlayher): turn this into a real REPL and share env.
    let mut env = Environment::new();

    let obj = evaluator::eval(node, &mut env).map_err(|err| err.to_string())?;
    println!("  - {}", obj);

    Ok(())
}

fn run_vm(node: ast::Node, print_bytecode: bool) -> Result<(), String> {
    let mut c = Compiler::new();
    c.compile(node).map_err(|err| err.to_string())?;
    let bc = c.bytecode();

    if print_bytecode {
        println!("{}", bc);
    }

    let start = time::Instant::now();

    let mut vm = Vm::new(bc);
    if let Err(err) = vm.run() {
        println!("error debug: {:?}\n", err);
        return Err(err.to_string());
    };

    let end = start.elapsed().as_secs_f64();

    println!("compiler/VM: ({}s elapsed)", end);

    // TODO(mdlayher): more output.
    println!("  - {:?}", vm.last_popped());

    Ok(())
}
