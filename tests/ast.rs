extern crate mdl_monkey;

use mdl_monkey::ast;

#[test]
fn ast_display() {
    let program = ast::Program {
        statements: vec![ast::Statement::Let(ast::LetStatement {
            name: ast::Identifier {
                value: "myVar".to_string(),
            },
            value: ast::Expression::Identifier(ast::Identifier {
                value: "anotherVar".to_string(),
            }),
        })],
    };

    assert_eq!(format!("{}", program), "let myVar = anotherVar;")
}
