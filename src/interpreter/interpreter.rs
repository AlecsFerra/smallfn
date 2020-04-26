use crate::interpreter::memory::Memory;
use crate::interpreter::value::Value;
use crate::language::grammar::ast::AST;

pub struct Interpreter {
    memory: Memory
}


impl Interpreter {
    pub fn new() -> Self {
        Self {
            memory: Memory::new()
        }
    }

    pub fn eval(&mut self, ast: AST) -> Value {
        match ast {
            //Trivial stuff
            AST::Variable(id) => self.memory.retrieve_value(id).unwrap(),
            AST::BooleanLiteral(bool) => Value::Bool(bool),
            AST::StringLiteral(str) => Value::String(str),
            AST::CharLiteral(char) => Value::Char(char),
            AST::IntegerLiteral(int) => Value::Integer(int),
            AST::FloatLiteral(float) => Value::Float(float),
            AST::FunctionDeclaration(id, params, _, ast) => {
                // Collecting the function identifiers needed for the application
                // It can discard the types because they were already checked by di static analyzer
                let params_id = params
                    .into_iter()
                    .map(|(id, _)| id)
                    .collect();
                self.memory.insert_value(id, Value::Function(params_id, *ast));
                // To make a function declaration return itself simply replace the last line with
                // the inserted value
                Value::Unit
            }
            AST::VariableDeclaration(id, _, val) => {
                let val = self.eval(*val);
                self.memory.insert_value(id, val);
                Value::Unit
            }
            AST::If(cond, t_branch, f_branch) => {
                let is_t = match self.eval(*cond) {
                    Value::Bool(bool) => bool,
                    _ => unreachable!("Runtime error (?) an if statement condition returned a non boolean value")
                };
                self.memory.create_frame();
                let ret = self.eval(*if is_t { t_branch } else { f_branch });
                self.memory.remove_frame();
                ret
            }
            AST::FunctionApplication(id, params) => {
                // Todo: Find a better way to load redefined functions
                let ret = if id == "eqi".to_string() {
                    let fst = match self.eval(params.get(0).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    let snd = match self.eval(params.get(1).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    //println!("internal: eq {}, {}", fst, snd);
                    Value::Bool(fst == snd)
                } else if id == "addi".to_string() {
                    let fst = match self.eval(params.get(0).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    let snd = match self.eval(params.get(1).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    //println!("internal: addi {}, {}", fst, snd);
                    Value::Integer(fst + snd)
                } else if id == "subi".to_string() {
                    let fst = match self.eval(params.get(0).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    let snd = match self.eval(params.get(1).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    //println!("internal: subi {}, {}", fst, snd);
                    Value::Integer(fst - snd)
                } else if id == "printi".to_string() {
                    let fst = match self.eval(params.get(0).unwrap().clone()) {
                        Value::Integer(i) => i,
                        _ => unreachable!()
                    };
                    println!("{}", fst);
                    Value::Unit
                } else {
                    let (params_id, fun) = match self.memory.retrieve_value(id).unwrap() {
                        Value::Function(params, ast) => (params, ast),
                        _ => unreachable!("Runtime error (?)")
                    };

                    // Evaluating the applied parameters in the outer scope
                    let evaluated_params: Vec<Value> = params
                        .into_iter()
                        .map(|expr| self.eval(expr))
                        .collect();
                    self.memory.create_frame();
                    for (id, val) in params_id.iter().zip(evaluated_params.iter()) {
                        self.memory.insert_value(id.clone(), val.clone());
                    }
                    let ret = self.eval(fun);
                    self.memory.remove_frame();
                    ret
                };
                ret
            }
            AST::Block(statements) => {
                let mut last_ret = Value::Unit;
                for statement in statements {
                    last_ret = self.eval(statement)
                }
                last_ret
            }
        }
    }
}