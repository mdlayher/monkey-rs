extern crate mdl_monkey;

use mdl_monkey::{ast, lexer, parser};

#[test]
fn parse_statements() {
    let tokens = lexer::Lexer::new(
        "
let five = 5;
let ten = 10;
return 5;
",
    )
    .lex();

    let mut parser = parser::Parser::new(tokens);

    let mut program = ast::Program::new();
    parser.parse(&mut program).unwrap();

    // TODO(mdlayher): finish parser and tests.
}
