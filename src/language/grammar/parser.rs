use std::iter::Peekable;
use std::slice::Iter;

use crate::language::tokenizer::token::{ Token, TokenKind };
use crate::language::grammar::ast::AST;
use crate::language::types::{ Type, KNOWN_TYPES };

macro_rules! expect {
    ($expected: pat, $expr: expr, $err: expr) => {{
        match $expr {
            Some(token) => match token.clone().kind {
                $expected => (),
                _         => return $err
            },
            _    => return $err
        }
    }};
    ($expr: expr, $err: expr) => {{
        match $expr {
            Some(token) => match token.clone().kind {
                TokenKind::Id(id) => id,
                _                 => return $err
            },
            _    => return $err
        }
    }};
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens: tokens.iter().peekable()
        }
    }
}

impl Parser<'_> {
    pub fn parse(&mut self) -> Result<AST, String> {
        self.parse_block()
    }

    fn parse_block(&mut self) -> Result<AST, String> {
        let mut statements = Vec::new();
        while let Some(parsed_statement) = self.parse_statement() {
            match parsed_statement {
                Err(msg) => return Err(msg),
                Ok(parsed) => statements.push(parsed)
            }
        }
        Ok(AST::Block(statements))
    }

    fn parse_statement(&mut self) -> Option<Result<AST, String>> {
        match self.tokens.peek() {
            None => None,
            Some(token) => match &token.kind {
                TokenKind::End => {
                    self.tokens.next();
                    None
                },
                TokenKind::Fn => Some(self.parse_function_declaration()),
                TokenKind::Let => Some(self.parse_variable_declaration()),
                _ => Some(self.parse_expression())
            }
        }
    }

    fn parse_function_declaration(&mut self) -> Result<AST, String> {
        self.tokens.next();
        let id = expect!(self.tokens.next(), Err(format!("Expected identifier")));
        expect!(TokenKind::LParen, self.tokens.next(), Err(format!("Expected (")));
        let mut params = Vec::new();
        while let Some(token) = self.tokens.next() {
            if token.kind == TokenKind::RParen {
                break;
            } else if token.kind == TokenKind::Coma {
                continue;
            }
            let id = expect!(Some(token), Err(format!("Expected identifier")));
            let ttype = match self.type_dec() {
                Some(ttype) => ttype,
                None => return Err(format!("Expected identifier type"))
            };
            params.push((id, ttype));
        }
        let ret_type = match self.type_dec() {
            Some(ttype) => ttype,
            _ => return Err(format!("Expected return type"))
        };
        expect!(TokenKind::Assign, self.tokens.next(), Err(format!("Expected =")));
        match self.tokens.peek() {
            None => Err(format!("Expected ( of  expression after =")),
            Some(token) => match token.clone().kind {
                TokenKind::Do => {
                    self.tokens.next();
                    let block = match self.parse_block() {
                        Ok(block) => block,
                        err => return err
                    };
                    Ok(AST::FunctionDeclaration(id, params, ret_type, Box::from(block)))
                },
                _ => {
                    let expr = match self.parse_expression() {
                        Ok(expr)  => expr,
                        Err(err) => return Err(err),
                    };
                    Ok(AST::FunctionDeclaration(id, params, ret_type, Box::from(expr)))
                }
            }
        }

    }

    fn type_dec(&mut self) -> Option<Type> {
        expect!(TokenKind::Colon, self.tokens.peek(), None);
        self.tokens.next();
        let ttype = expect!(self.tokens.next(), None);
        match KNOWN_TYPES.get(ttype.as_str()) {
            Some(ttype) => Some(ttype.clone()),
            _           => None
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<AST, String> {
        self.tokens.next();
        let id = expect!(self.tokens.next(), Err(format!("Expected identifier")));
        let maybe_type = self.type_dec();
        expect!(TokenKind::Assign, self.tokens.next(), Err(format!("Expected =")));
        let expr = match self.parse_expression() {
            Ok(expr) => expr,
            Err(err)         => return Err(err),
        };
        Ok(AST::VariableDeclaration(id, maybe_type, Box::from(expr)))
    }

    fn parse_expression(&mut self) -> Result<AST, String> {
        match self.tokens.next() {
            None => Err(format!("Expected expression")),
            Some(token) => {
                match &token.kind {
                    TokenKind::BooleanLiteral(bool) => Ok(AST::BooleanLiteral(*bool)),
                    TokenKind::StringLiteral(string) => Ok(AST::StringLiteral(string.to_string())),
                    TokenKind::CharLiteral(char) => Ok(AST::CharLiteral(*char)),
                    TokenKind::IntegerLiteral(int) => Ok(AST::IntegerLiteral(*int)),
                    TokenKind::FloatLiteral(float) => Ok(AST::FloatLiteral(*float)),
                    TokenKind::Id(id) => match self.tokens.peek() {
                        Some(next) => {
                            if next.clone().kind == TokenKind::LParen {
                                self.tokens.next();
                                let mut params = Vec::new();
                                while let Some(token) = self.tokens.peek() {
                                    if token.kind == TokenKind::RParen {
                                        self.tokens.next();
                                        break;
                                    } else if token.kind == TokenKind::Coma {
                                        self.tokens.next();
                                        continue;
                                    }
                                    let param = match self.parse_expression() {
                                        Ok(expr) => expr,
                                        err  => return err
                                    };
                                    params.push(param);
                                }
                                return Ok(AST::FunctionApplication(id.to_string(), params))
                            } else {
                                Ok(AST::Variable(id.to_string()))
                            }
                        },
                        None => Ok(AST::Variable(id.to_string()))
                    },
                    tok => Err(format!("Unexpected token"))
                }
            }
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_declaration() {
        let tokens = vec![
            Token::new(TokenKind::Fn, 0, 0),
            Token::new(TokenKind::Id("foo".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::Id("bar".to_string()), 0, 0),
            Token::new(TokenKind::Colon, 0, 0),
            Token::new(TokenKind::Id("Int".to_string()), 0, 0),
            Token::new(TokenKind::Coma, 0, 0),
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
                Box::from(AST::Variable("bar".to_string()))
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
                Box::from(AST::BooleanLiteral(true))
            )
        ));
    }

    #[test]
    fn test_expression() {
        let tokens = vec![
            Token::new(TokenKind::Id("foo".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::Coma, 0, 0),
            Token::new(TokenKind::Id("bar".to_string()), 0, 0),
            Token::new(TokenKind::LParen, 0, 0),
            Token::new(TokenKind::IntegerLiteral(42), 0, 0),
            Token::new(TokenKind::Coma, 0, 0),
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
                    ]
                )
            ]
        )));
    }

}

