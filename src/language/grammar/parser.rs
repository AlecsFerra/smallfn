use std::iter::Peekable;
use std::slice::Iter;

use crate::language::grammar::ast::AST;
use crate::language::tokenizer::token::{Token, TokenKind};
use crate::language::types::{KNOWN_TYPES, Type};

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

macro_rules! get_or_propagate {
    ($provider: expr) => {{
        match $provider {
            Ok(data) => data,
            Err(err) => return Err(err),
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
        let mut statements: Vec<AST> = Vec::new();
        while let Some(parsed_statement) = self.parse_statement() {
            statements.push(get_or_propagate!(parsed_statement));
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
                }
                TokenKind::Fn => Some(self.parse_function_declaration()),
                TokenKind::Let => Some(self.parse_variable_declaration()),
                _ => Some(self.parse_block_or_expression())
            }
        }
    }

    pub(crate) fn parse_function_declaration(&mut self) -> Result<AST, String> {
        self.tokens.next();
        let id = expect!(self.tokens.next(), Err(format!("Expected identifier")));
        expect!(TokenKind::LParen, self.tokens.next(), Err(format!("Expected (")));

        let mut params = Vec::new();
        while let Some(token) = self.tokens.next() {
            if token.kind == TokenKind::RParen { break }
            let id = expect!(Some(token), Err(format!("Expected identifier")));
            let id_type = get_or_propagate!(self.parse_type_declaration());
            params.push((id, id_type));
        }

        let ret_type = get_or_propagate!(self.parse_type_declaration());
        expect!(TokenKind::Assign, self.tokens.next(), Err(format!("Expected =")));
        Ok(AST::FunctionDeclaration(id, params, ret_type, Box::from(get_or_propagate!(self.parse_block_or_expression()))))
    }

    fn parse_type_declaration(&mut self) -> Result<Type, String> {
        expect!(TokenKind::Colon, self.tokens.peek(), Err(format!("Expected :")));
        self.tokens.next();
        self.parse_type()
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        match self.tokens.next() {
            None => Err(format!("cannot find type")),
            Some(token) => match token.clone().kind {
                TokenKind::Id(id) => Ok(KNOWN_TYPES.get(id.as_str()).unwrap().clone()),
                TokenKind::LParen => {
                    let mut params = Vec::new();
                    while let Some(token) = self.tokens.peek() {
                        if token.kind == TokenKind::RParen { break }
                        params.push(get_or_propagate!(self.parse_type()));
                    }
                    self.tokens.next();
                    expect!(TokenKind::To, self.tokens.next(), Err(format!("Expected to")));
                    let ret = get_or_propagate!(self.parse_type());
                    Ok(Type::Function(params, Box::from(ret)))
                },
                err => { println!("unm: {:?}", err); Err(format!("cannot find type")) }
            }
        }
    }

    pub(crate) fn parse_variable_declaration(&mut self) -> Result<AST, String> {
        self.tokens.next();
        let id = expect!(self.tokens.next(), Err(format!("Expected identifier")));
        let maybe_type = self.parse_type_declaration().ok();
        expect!(TokenKind::Assign, self.tokens.next(), Err(format!("Expected =")));
        let expr = get_or_propagate!(self.parse_block_or_expression());
        Ok(AST::VariableDeclaration(id, maybe_type, Box::from(expr)))
    }

    fn parse_block_or_expression(&mut self) -> Result<AST, String> {
        match self.tokens.peek() {
            None => Err(format!("Expected ( of  expression after =")),
            Some(token) => match token.clone().kind {
                TokenKind::Do => {
                    self.tokens.next();
                    Ok(get_or_propagate!(self.parse_block()))
                }
                _ => Ok(get_or_propagate!(self.parse_expression()))
            }
        }
    }

    pub(crate) fn parse_expression(&mut self) -> Result<AST, String> {
        match self.tokens.next() {
            None => Err(format!("Expected expression")),
            Some(token) => {
                match &token.kind {
                    // Literals
                    TokenKind::BooleanLiteral(bool) => Ok(AST::BooleanLiteral(*bool)),
                    TokenKind::StringLiteral(string) => Ok(AST::StringLiteral(string.to_string())),
                    TokenKind::CharLiteral(char) => Ok(AST::CharLiteral(*char)),
                    TokenKind::IntegerLiteral(int) => Ok(AST::IntegerLiteral(*int)),
                    TokenKind::FloatLiteral(float) => Ok(AST::FloatLiteral(*float)),
                    TokenKind::If => {
                        let cond = get_or_propagate!(self.parse_block_or_expression());
                        expect!(TokenKind::Then, self.tokens.next(), Err(format!("Expected then")));
                        let t_body = get_or_propagate!(self.parse_block_or_expression());
                        expect!(TokenKind::Else, self.tokens.next(), Err(format!("Expected then")));
                        let e_body = get_or_propagate!(self.parse_block_or_expression());
                        Ok(AST::If(Box::from(cond), Box::from(t_body), Box::from(e_body)))
                    }
                    // See if is a variable or a function application
                    TokenKind::Id(id) => match self.tokens.peek() {
                        Some(next) => if next.clone().kind == TokenKind::LParen {
                            self.tokens.next();

                            let mut params = Vec::new();
                            while let Some(token) = self.tokens.peek() {
                                if token.kind == TokenKind::RParen { break }
                                params.push(get_or_propagate!(self.parse_block_or_expression()));
                            }

                            expect!(TokenKind::RParen, self.tokens.next(), Err(format!("Expected )")));
                            Ok(AST::FunctionApplication(id.to_string(), params))
                        } else {
                            Ok(AST::Variable(id.to_string()))
                        }
                        None => Ok(AST::Variable(id.to_string()))
                    },
                    unx => {
                        println!("{:?}", unx);
                        Err(format!("Unexpected token in expression"))
                    }
                }
            }
        }
    }
}
