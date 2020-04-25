use crate::interpreter::interpreter::Interpreter;
use crate::language::grammar::parser::Parser;
use crate::language::tokenizer::lexer::Lexer;
use crate::language::validation::analyzer::StaticAnalyzer;

pub mod language;
pub mod interpreter;

fn main() {
    let prog = "
      fn mul(a: Int b: Int): Int =
        if eqi(b 0) then 0
        else addi(a mul(a subi(b 1)))
      fn apply_dup(a: Int fnc: (Int Int) to Int): Int =
        fnc(a a)
      printi(apply_dup(3 mul))
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
