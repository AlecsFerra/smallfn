use crate::language::types::Type;

#[derive(Debug, PartialEq)]
pub enum AST {
    Block(Vec<AST>),
    FunctionDeclaration(String, Vec<(String, Type)>, Type, Box<AST>),
    VariableDeclaration(String, Option<Type>, Box<AST>),
    FunctionApplication(String, Vec<AST>),
    If(Box<AST>, Box<AST>, Box<AST>),
    Variable(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(String),
    CharLiteral(char),
}