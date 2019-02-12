extern crate mdl_monkey;

use mdl_monkey::{ast, lexer, parser, token};

#[test]
fn parse_statements() {
    let _prog = parse(
        "
let five = 5;
let ten = 10;
return 5;
",
    );

    // TODO(mdlayher): finish parser and tests.
}

#[test]
fn parse_identifier_expression() {
    let prog = parse("foobar;");

    assert_eq!(prog.statements.len(), 1);

    let id = if let ast::Statement::Expression(expr) = &prog.statements[0] {
        if let ast::Expression::Identifier(id) = expr {
            id.to_string()
        } else {
            panic!("not an identifier expression");
        }
    } else {
        panic!("not an expression statement");
    };

    assert_eq!("foobar", id);
}

#[test]
fn parse_integer_literal_expression() {
    let prog = parse("5;");

    assert_eq!(prog.statements.len(), 1);

    let want = token::Integer {
        radix: token::Radix::Decimal,
        value: 5,
    };

    let got = if let ast::Statement::Expression(expr) = &prog.statements[0] {
        if let ast::Expression::Integer(int) = expr {
            int
        } else {
            panic!("not an integer expression");
        }
    } else {
        panic!("not an expression statement");
    };

    assert_eq!(want, *got);
}

fn parse(input: &str) -> ast::Program {
    let l = lexer::Lexer::new(input);

    let mut p = parser::Parser::new(l).expect("failed to create parser");

    p.parse().expect("failed to parse program")
}
