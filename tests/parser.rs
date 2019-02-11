extern crate mdl_monkey;

use mdl_monkey::{lexer, parser};

#[test]
fn parse_statements() {
    let lexer = lexer::Lexer::new(
        "
let five = 5;
let ten = 10;
return 5;
",
    );

    let mut parser = parser::Parser::new(lexer).expect("failed to create parser");

    let _prog = parser.parse().expect("failed to parse program");

    // TODO(mdlayher): finish parser and tests.
}
