pub mod ast;
pub mod parser;

#[cfg(test)]
mod tests {
    use crate::language::grammar::ast::AST;
    use crate::language::grammar::parser::Parser;
    use crate::language::tokenizer::token::{Token, TokenKind};
    use crate::language::types::Type;

    #[test]
    fn test_function_declaration() {
        let tokens = vec![
            Token::new(TokenKind::Fn, 0, 0),
            Token::new(TokenKind::Id("foo".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::Id("bar".to_string()), 0, 0),
            Token::new(TokenKind::Colon, 0, 0),
            Token::new(TokenKind::Id("Int".to_string()), 0, 0),
            Token::new(TokenKind::Id("bazz".to_string()), 0, 0),
            Token::new(TokenKind::Colon, 0, 0),
            Token::new(TokenKind::Id("Float".to_string()), 0, 0),
            Token::new(TokenKind::RParen, 0, 0),
            Token::new(TokenKind::Colon, 0, 0),
            Token::new(TokenKind::Id("Bool".to_string()), 0, 0),
            Token::new(TokenKind::Assign, 0, 0),
            Token::new(TokenKind::BooleanLiteral(true), 0, 0),
        ];
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse_function_declaration(), Ok(
            AST::FunctionDeclaration("foo".to_string(),
                                     vec!(("bar".to_string(), Type::Integer),
                                          ("bazz".to_string(), Type::Float)),
                                     Type::Boolean,
                                     Box::from(AST::BooleanLiteral(true)))));
    }

    #[test]
    fn test_variable_declaration() {
        let tokens = vec![
            Token::new(TokenKind::Let, 0, 0),
            Token::new(TokenKind::Id("foo".to_string()), 0, 0),
            Token::new(TokenKind::Assign, 0, 0),
            Token::new(TokenKind::Id("bar".to_string()), 0, 0),
        ];
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse_variable_declaration(), Ok(
            AST::VariableDeclaration(
                "foo".to_string(),
                None,
                Box::from(AST::Variable("bar".to_string())),
            )
        ));
    }

    #[test]
    fn test_variable_declaration_typed() {
        let tokens = vec![
            Token::new(TokenKind::Let, 0, 0),
            Token::new(TokenKind::Id("foo".to_string()), 0, 0),
            Token::new(TokenKind::Colon, 0, 0),
            Token::new(TokenKind::Id("Bool".to_string()), 0, 0),
            Token::new(TokenKind::Assign, 0, 0),
            Token::new(TokenKind::BooleanLiteral(true), 0, 0),
        ];
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse_variable_declaration(), Ok(
            AST::VariableDeclaration(
                "foo".to_string(),
                Some(Type::Boolean),
                Box::from(AST::BooleanLiteral(true)),
            )
        ));
    }

    #[test]
    fn test_expression() {
        let tokens = vec![
            Token::new(TokenKind::Id("foo".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::Id("bar".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::RParen, 0, 0),
            Token::new(TokenKind::RParen, 0, 0),
        ];
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse_expression(), Ok(AST::FunctionApplication(
            "foo".to_string(),
            vec![
                AST::IntegerLiteral(42),
                AST::FunctionApplication(
                    "bar".to_string(),
                    vec![
                        AST::IntegerLiteral(42),
                        AST::IntegerLiteral(42),
                    ],
                )
            ],
        )));
    }

    #[test]
    fn test_if() {
        let tokens = vec![
            Token::new(TokenKind::If, 0, 0),
            Token::new(TokenKind::Id("eq".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::RParen, 0, 0),
            Token::new(TokenKind::Then, 0, 0),
            Token::new(TokenKind::Do, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::End, 0, 0),
            Token::new(TokenKind::Else, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
        ];
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.parse_expression(), Ok(
            AST::If(
                Box::from(AST::FunctionApplication("eq".to_string(), vec![AST::IntegerLiteral(42), AST::IntegerLiteral(42)])),
                Box::from(AST::Block(vec![AST::IntegerLiteral(42), AST::IntegerLiteral(42)])),
                Box::from(AST::IntegerLiteral(42))))
        );
    }
}
