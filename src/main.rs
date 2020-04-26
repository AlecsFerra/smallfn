use crate::interpreter::interpreter::Interpreter;
use crate::language::grammar::parser::Parser;
use crate::language::tokenizer::lexer::Lexer;
use crate::language::validation::analyzer::StaticAnalyzer;

pub mod language;
pub mod interpreter;
 /**
 fn */
fn main() {
     let prog = "
      fn apply_dup(a: Int fnc: (Int Int) to Int): Int =
        fnc(a a)
      fn mul(a: Int b: Int): Int =
        if eqi(b 0) then 0
        else addi(a mul(a subi(b 1)))
      fn get_mul(): (Int Int) to Int = mul
      printi(apply_dup(3 get_mul()))
      printi(apply_dup(4 get_mul()))

      fn fib(in: Int): Int = do
        printi(in)
        if eqi(in 0) then 0
        else if eqi(in 1) then 1
        else addi(fib(subi(in 1)) fib(subi(in 2)))
      end
      printi(fib(8))
    ".to_string();
     let lexer = Lexer::new(&prog);
     let tokens = lexer.take_while(|x| x.is_ok())
        .map(|x| x.unwrap())
        .collect::<Vec<_>>();
     let mut parser = Parser::new(&tokens);
    let parsed = parser.parse().unwrap();
    let mut analyzer = StaticAnalyzer::new();
    analyzer.analyze(parsed.clone()).unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.eval(parsed);
}
