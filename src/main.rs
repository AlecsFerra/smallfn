use crate::language::tokenizer::token::TokenKind;

pub mod language;


fn main() {
    TokenKind::Id("Lol".parse().unwrap());
    println!("Hello, world!");
}
