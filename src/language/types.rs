use std::collections::HashMap;

use lazy_static::lazy_static;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Integer,
    Float,
    Char,
    String,
    Boolean,
    Unit,
    Function(Vec<Type>, Box<Type>),
}

lazy_static! {
    pub static ref KNOWN_TYPES: HashMap<&'static str, Type> = {
        let mut m = HashMap::new();
        m.insert("Int",    Type::Integer);
        m.insert("Float",  Type::Float);
        m.insert("Char",   Type::Char);
        m.insert("String", Type::String);
        m.insert("Bool",   Type::Boolean);
        m.insert("Unit",   Type::Unit);
        m
    };
}
