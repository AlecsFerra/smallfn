use crate::language::grammar::ast::AST;
use crate::language::types::Type;
use crate::language::validation::symbol_table::SymbolTable;

macro_rules! propagate {
    ($option: expr) => {{
        match $option {
            Ok(ttype) => ttype,
            err  => return err
        }
    }};
    ($option: expr, $err: expr) => {{
        match $option {
            Some(ttype) => ttype,
            _  => return $err
        }
    }}
}

pub struct StaticAnalyzer {
    symbol_table: SymbolTable
}

impl StaticAnalyzer {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();
        symbol_table.declare(
            "eqi".to_string(),
            Type::Function(vec![Type::Integer, Type::Integer],
                           Box::from(Type::Boolean)));
        symbol_table.declare(
            "addi".to_string(),
            Type::Function(vec![Type::Integer, Type::Integer],
                           Box::from(Type::Integer)));
        symbol_table.declare(
            "subi".to_string(),
            Type::Function(vec![Type::Integer, Type::Integer],
                           Box::from(Type::Integer)));
        symbol_table.declare(
            "printi".to_string(),
            Type::Function(vec![Type::Integer],
                           Box::from(Type::Unit)));
        Self {
            symbol_table
        }
    }

    pub fn analyze(&mut self, ast: AST) -> Result<Type, String> {
        match ast {
            AST::Block(block) => {
                let mut last_type = Type::Unit;
                for statement in block {
                    last_type = propagate!(self.analyze(statement));
                }
                Ok(last_type)
            }
            AST::If(clause, t_arm, f_arm) => {
                if propagate!(self.analyze(*clause)) != Type::Boolean {
                    return Err(String::from("If clause must be of boolean type"));
                }
                self.symbol_table.create_frame();
                let t_type = propagate!(self.analyze(*t_arm));
                self.symbol_table.remove_frame();
                self.symbol_table.create_frame();
                let r_type = propagate!(self.analyze(*f_arm));
                self.symbol_table.remove_frame();
                if t_type != r_type {
                    Err(String::from("True and false arm of an if clause must return the same type"))
                } else {
                    Ok(t_type)
                }
            }
            AST::FunctionDeclaration(id, params, ret, def) => {
                if self.symbol_table.retrieve_type(id.clone()).is_some() {
                    return Err(format!("{} was already declared in this scope", id));
                }

                let mut param_t = Vec::new();
                for (_, ttype) in params.iter() {
                    param_t.push(ttype.clone())
                }

                let fn_type = Type::Function(
                    param_t,
                    Box::from(ret.clone()),
                );

                self.symbol_table.declare(id.clone(), fn_type.clone());

                self.symbol_table.create_frame();
                for (id, ttype) in params {
                    self.symbol_table.declare(id, ttype);
                }
                let rel_ret_val = propagate!(self.analyze(*def));
                if rel_ret_val != ret {
                    return Err(format!("{} has unmatched declaration and return type", id));
                }
                self.symbol_table.remove_frame();
                Ok(Type::Unit)
            }
            AST::FunctionApplication(id, args) => {
                let fn_type = propagate!(self.symbol_table.retrieve_type(id.clone()),
                                        Err(format!("Use of undeclared {}", id)));
                let (params, return_type) = match fn_type {
                    Type::Function(p, r) => (p, *r),
                    _ => return Err(format!("Function application {} that isn't a function", id))
                };
                if params.len() != args.len() {
                    return Err(format!("Function {} require {} arguments but found {}",
                                       id, params.len(), args.len()));
                }
                let mut args_t = Vec::new();
                for arg in args {
                    args_t.push(propagate!(self.analyze(arg)))
                }
                if args_t != params {
                    Err(format!("Function {} has mismatched types", id))
                } else {
                    Ok(return_type)
                }
            }
            AST::VariableDeclaration(id, opt_type, expr) => {
                if self.symbol_table.retrieve_type(id.clone()).is_some() {
                    return Err(format!("{} was already declared in this scope", id));
                }
                let expr_type = propagate!(self.analyze(*expr));
                match opt_type {
                    None => {
                        self.symbol_table.declare(id.clone(), expr_type);
                        Ok(Type::Unit)
                    }
                    Some(expected_type) => {
                        if expected_type != expr_type {
                            return Err(format!("{} was declaration type is different from the assigned value", id));
                        }
                        self.symbol_table.declare(id.clone(), expr_type);
                        Ok(Type::Unit)
                    }
                }
            }
            AST::Variable(id) => Ok(propagate!(self.symbol_table.retrieve_type(id.clone()),
                                        Err(format!("Use of undeclared variable {}", id)))),
            AST::IntegerLiteral(_) => Ok(Type::Integer),
            AST::FloatLiteral(_) => Ok(Type::Float),
            AST::CharLiteral(_) => Ok(Type::Char),
            AST::StringLiteral(_) => Ok(Type::String),
            AST::BooleanLiteral(_) => Ok(Type::Boolean),
        }
    }
}