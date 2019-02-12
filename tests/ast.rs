extern crate mdl_monkey;

use mdl_monkey::ast;

#[test]
fn ast_display() {
    let program = ast::Program {
        statements: vec![ast::Statement::Let(ast::LetStatement {
            name: "myVar".to_string(),
            value: ast::Expression::Identifier("anotherVar".to_string()),
        })],
    };

    assert_eq!(format!("{}", program), "let myVar = anotherVar;")
}
