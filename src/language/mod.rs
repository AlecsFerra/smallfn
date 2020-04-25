pub mod tokenizer;
pub mod grammar;
pub mod types;
pub mod validation;

#[cfg(test)]
mod tests {
    use crate::language::grammar::ast::AST;
    use crate::language::grammar::parser::Parser;
    use crate::language::tokenizer::lexer::Lexer;
    use crate::language::types::Type;

    #[test]
    fn test_no_block() {
        let str = "
        fn foo(bar: Int baz: Float): Int = add(bar baz)
        fn main(): Unit = add(12 foo(32 22.4))
        ".to_string();
        let lexer = Lexer::new(&str);
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
                             AST::Variable("baz".to_string())],
                    )),
                ),
                AST::FunctionDeclaration(
                    "main".to_string(),
                    vec![],
                    Type::Unit,
                    Box::from(AST::FunctionApplication(
                        "add".to_string(),
                        vec![AST::IntegerLiteral(12),
                             AST::FunctionApplication(
                                 "foo".to_string(),
                                 vec![AST::IntegerLiteral(32),
                                      AST::FloatLiteral(22.4)])
                        ],
                    )))
            ])
        ));
    }

    #[test]
    fn test_block() {
        let str = "
        fn foo(bar: Int baz: Float): Int = add(bar baz)
        fn main(): Unit = do
            let a: Int = 11
            let b = true
            add(6 6)
        end
        fn bar(): Int = 33
        ".to_string();
        let lexer = Lexer::new(&str);
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
                ])),
            ),
            AST::FunctionDeclaration(
                "main".to_string(),
                vec![],
                Type::Unit,
                Box::from(AST::Block(vec![
                    AST::VariableDeclaration("a".to_string(), Some(Type::Integer), Box::from(AST::IntegerLiteral(11))),
                    AST::VariableDeclaration("b".to_string(), None, Box::from(AST::BooleanLiteral(true))),
                    AST::FunctionApplication("add".to_string(), vec![AST::IntegerLiteral(6), AST::IntegerLiteral(6)])]
                ))),
            AST::FunctionDeclaration("bar".to_string(), vec![], Type::Integer, Box::from(AST::IntegerLiteral(33)))])))
    }

    #[test]
    fn test_if_in_fn() {
        println!("starting test");
        let str = "
        fn fact(in: Int): Int =
            if eq(0 in) then
                1
            else
                add(in fact(sub(in 1)))
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse(), Ok(AST::Block(vec![
            AST::FunctionDeclaration(
                "fact".to_string(),
                vec![("in".to_string(), Type::Integer)],
                Type::Integer,
                Box::from(AST::If(
                    Box::from(AST::FunctionApplication(
                        "eq".to_string(),
                        vec![AST::IntegerLiteral(0), AST::Variable("in".to_string())])),
                    Box::from(AST::IntegerLiteral(1)),
                    Box::from(AST::FunctionApplication(
                        "add".to_string(),
                        vec![AST::Variable("in".to_string()),
                             AST::FunctionApplication(
                                 "fact".to_string(),
                                 vec![AST::FunctionApplication("sub".to_string(), vec![AST::Variable("in".to_string()), AST::IntegerLiteral(1)])])
                        ])))))
        ])))
    }

    #[test]
    fn functional_test() {
        println!("starting test");
        let str = "
        fn funny(in: (Int (Float) to Int) to Bool): Int = 33
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse(), Ok(
            AST::Block(vec![
                AST::FunctionDeclaration(
                    "funny".to_string(), vec![
                        ("in".to_string(), Type::Function(vec![
                            Type::Integer,
                            Type::Function(vec![Type::Float], Box::from(Type::Integer))], Box::from(Type::Boolean)))],
                    Type::Integer,
                    Box::from(AST::IntegerLiteral(33)))])))
    }
}