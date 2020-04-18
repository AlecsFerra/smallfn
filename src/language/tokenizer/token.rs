use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Id(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(String),
    CharLiteral(char),
    Let,
    Fn,
    Colon,
    LParen,
    RParen,
    Coma,
    Assign
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
    pub char: u32,
}

impl Token {
    pub fn new(kind: TokenKind, line: u32, char: u32) -> Self {
        Self {
            kind,
            line,
            char,
        }
    }
}

lazy_static! {
    pub static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("true",  TokenKind::BooleanLiteral(true));
        m.insert("false", TokenKind::BooleanLiteral(false));
        m.insert("let",   TokenKind::Let);
        m.insert("fn",    TokenKind::Fn);
        m.insert(",",     TokenKind::Colon);
        m.insert("(",     TokenKind::LParen);
        m.insert(")",     TokenKind::RParen);
        m.insert(",",     TokenKind::Coma);
        m.insert("=",     TokenKind::Assign);
        m
    };
}
