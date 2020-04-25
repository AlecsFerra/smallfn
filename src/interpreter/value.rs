use crate::language::grammar::ast::AST;

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Function(Vec<String>, AST),
    Unit,
}