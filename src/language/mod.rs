pub mod tokenizer;
pub mod grammar;
pub mod types;

#[cfg(test)]
mod tests {
    use crate::language::tokenizer::lexer::Lexer;
    use crate::language::grammar::parser::Parser;
    use crate::language::grammar::ast::AST;
    use crate::language::types::Type;

    #[test]
    fn test_no_block() {
        let str = "
        fn foo(bar: Int, baz: Float): Int = add(bar, baz)
        fn main(): Int = add(12, foo(32, 22.4))
        ".to_string();
        let mut lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
                                     .map(|x| x.unwrap())
                                     .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse(), Ok(
            AST::Block(vec![
                AST::FunctionDeclaration(
                    "foo".to_string(),
                    vec![("bar".to_string(), Type::Integer),
                         ("baz".to_string(), Type::Float)],
                    Type::Integer,
                    Box::from(AST::FunctionApplication(
                        "add".to_string(),
                        vec![AST::Variable("bar".to_string()),
                             AST::Variable("baz".to_string())]
                    ))
                ),
                AST::FunctionDeclaration(
                    "main".to_string(),
                    vec![],
                    Type::Integer,
                    Box::from(AST::FunctionApplication(
                        "add".to_string(),
                        vec![AST::IntegerLiteral(12),
                             AST::FunctionApplication(
                                 "foo".to_string(),
                                 vec![AST::IntegerLiteral(32),
                                      AST::FloatLiteral(22.4)])
                        ]
                    )))
            ])
        ));
    }

    #[test]
    fn test_block() {
        let str = "
        fn foo(bar: Int, baz: Float): Int = add(bar, baz)
        fn main(): Int = do
            let a: Int = 11
            let b = true
        end
        fn bar(): Int = 33
        ".to_string();
        let mut lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse(), Ok(AST::Block(vec![
            AST::FunctionDeclaration(
                "foo".to_string(),
                vec![("bar".to_string(), Type::Integer), ("baz".to_string(), Type::Float)],
                Type::Integer,
                Box::from(AST::FunctionApplication("add".to_string(), vec![
                    AST::Variable("bar".to_string()),
                    AST::Variable("baz".to_string())
                ]))
            ),
            AST::FunctionDeclaration(
                "main".to_string(),
                vec![],
                Type::Integer,
                Box::from(AST::Block(vec![
                    AST::VariableDeclaration("a".to_string(), Some(Type::Integer), Box::from(AST::IntegerLiteral(11))),
                    AST::VariableDeclaration("b".to_string(), None, Box::from(AST::BooleanLiteral(true)))]
                ))),
            AST::FunctionDeclaration("bar".to_string(), vec![], Type::Integer, Box::from(AST::IntegerLiteral(33)))])))
    }
    /*
    Ok(Block([
        FunctionDeclaration("foo", [("bar", Integer), ("baz", Float)], Integer, FunctionApplication("add", [Variable("bar"), Variable("baz")])),
        FunctionDeclaration("main", [], Integer, Block([
            VariableDeclaration("a", Some(Integer), FunctionApplication("add", [IntegerLiteral(12), FunctionApplication("foo", [IntegerLiteral(32), FloatLiteral(22.4)])])),
            VariableDeclaration("b", None, BooleanLiteral(true))]))]))

    */
}