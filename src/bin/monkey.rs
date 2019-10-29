extern crate getopts;
extern crate mdl_monkey;

use mdl_monkey::ast;
use mdl_monkey::code;
use mdl_monkey::compiler::Compiler;
use mdl_monkey::evaluator;
use mdl_monkey::lexer::Lexer;
use mdl_monkey::object::Environment;
use mdl_monkey::parser::Parser;
use mdl_monkey::token::Token;
use mdl_monkey::vm::{self, Vm};

use getopts::Options;
use std::env;

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
        let ins = code::Instructions::parse(&bc.instructions).map_err(|err| err.to_string())?;

        println!("constants:");
        for (i, con) in bc.constants.iter().enumerate() {
            println!("{:02}: {}", i, con);
        }

        println!("\nbytecode:\n\n{}", ins);
    }

    let mut stack = vm::new_stack();
    let mut vm = Vm::new(&mut stack, bc);
    vm.run().map_err(|err| err.to_string())?;

    println!("compiler/VM:");

    // TODO(mdlayher): more output.
    println!("  - {:?}", vm.last_popped());

    Ok(())
}
