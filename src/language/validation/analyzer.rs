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
                // The work of declaring a new scope is done by if statements or functions
                // a block return the last value computed in itself, if the block is empty returns
                // a unit instance
                let mut last_type = Type::Unit;
                for statement in block {
                    last_type = propagate!(self.analyze(statement));
                }
                Ok(last_type)
            }
            AST::If(clause, t_arm, f_arm) => {
                // Checking if the if clause is of type Boolean
                if propagate!(self.analyze(*clause)) != Type::Boolean {
                    return Err(String::from("If clause must be of boolean type"));
                }

                // Creating a new frame to check the type returned by the true arm of the if
                self.symbol_table.create_frame();
                let t_type = propagate!(self.analyze(*t_arm));
                self.symbol_table.remove_frame();

                // Same thing for the false arm
                self.symbol_table.create_frame();
                let r_type = propagate!(self.analyze(*f_arm));
                self.symbol_table.remove_frame();

                // The if statements arms must have the same return type
                if t_type != r_type {
                    Err(String::from("True and false arm of an if clause must return the same type"))
                } else {
                    Ok(t_type)
                }
            }
            AST::FunctionDeclaration(id, params, ret, def) => {
                // Construct the function signature and try to declare it in the current scope
                let param_t = params
                    .iter()
                    .map(|(_, t)| t)
                    .map(|x| x.clone())
                    .collect();
                let fn_type = Type::Function(
                    param_t,
                    Box::from(ret.clone()),
                );
                if !self.symbol_table.declare(id.clone(), fn_type.clone()) {
                    return Err(format!("{} was already declared in this scope", id));
                }

                // Create a new function frame then declare all the function arguments and check if
                // the function body return the same type declared in the signature
                self.symbol_table.create_frame();
                for (id, t_type) in params {
                    self.symbol_table.declare(id, t_type);
                }
                let rel_ret_val = propagate!(self.analyze(*def));
                if rel_ret_val != ret {
                    return Err(format!("{} has unmatched declaration and return type", id));
                }
                self.symbol_table.remove_frame();
                // A function declaration return the type unit maybe in the future it will return
                // the function itself(?), simply use the line Ok(fn_type)
                Ok(Type::Unit)
            }
            AST::FunctionApplication(id, args) => {
                // Getting from the scope the signature of the applicant
                let signature = propagate!(self.symbol_table.retrieve_type(id.clone()),
                                        Err(format!("Use of undeclared {}", id)));

                // Checking if the type corresponds to function and try to retrieve parameters and
                // return type
                let (params, return_type) = match signature {
                    Type::Function(p, r) => (p, *r),
                    _ => return Err(format!("Function application {} that isn't a function", id))
                };

                // No semi application supported
                if params.len() != args.len() {
                    return Err(format!("Function {} require {} arguments but found {}",
                                       id, params.len(), args.len()));
                }

                // Collecting arguments types
                let mut args_t = Vec::new();
                for arg in args {
                    args_t.push(propagate!(self.analyze(arg)))
                }

                // Check if function parameters and application are the same
                if args_t != params {
                    Err(format!("Function {} has mismatched types", id))
                } else {
                    Ok(return_type)
                }
            }
            AST::VariableDeclaration(id, opt_type, expr) => {
                // Retrieve the type of the assigned expression
                let expr_type = propagate!(self.analyze(*expr));

                // If the type was specified and it's different from the expression one error
                if opt_type.is_some() && expr_type != opt_type.unwrap() {
                    return Err(format!("{} was specified of type different from the expression one",
                                        id))
                }

                // Check if was already declared in this scope
                if !self.symbol_table.declare(id.clone(), expr_type.clone()) {
                    Err(format!("{} was already declared in this scope", id))
                } else {
                    Ok(expr_type)
                }
            }
            // Trivial
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