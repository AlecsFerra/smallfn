pub mod analyzer;
pub mod symbol_table;

#[cfg(test)]
mod tests {
    use crate::language::grammar::parser::Parser;
    use crate::language::tokenizer::lexer::Lexer;
    use crate::language::validation::analyzer::StaticAnalyzer;
    use crate::language::types::Type;

    #[test]
    fn fib_test() {
        let str = "
        fn eq(a: Int b: Int): Bool = true
        fn add(a: Int b: Int): Int = 3
        fn sub(a: Int b: Int): Int = 3
        fn fib(in: Int): Int = if eq(0 in) then 1 else add(in fib(sub(in 1)))
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        let mut analyzer = StaticAnalyzer::new();
        assert_eq!(analyzer.analyze(ast.unwrap()), Ok(Type::Unit))
    }

    #[test]
    fn non_declared() {
        let str = "
        fn eq(a: Int b: Int): Bool = true
        fn add(a: Int b: Int): Int = 3
        fn sub(a: Int b: Int): Int = 3
        fn fib(in: Int): Int = if eq(0 in) then 1 else add(in fib(sub(n 1)))
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        let mut analyzer = StaticAnalyzer::new();
        assert_eq!(analyzer.analyze(ast.unwrap()), Err(String::from("Use of undeclared variable n")))
    }

    #[test]
    fn mismatched_types() {
        let str = "
        fn eq(a: Int b: Int): Bool = true
        fn add(a: Int b: Int): Int = 3
        fn sub(a: Int b: Int): Int = 3
        fn fib(in: Int): Int = if eq(true in) then 1 else add(in fib(sub(n 1)))
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        let mut analyzer = StaticAnalyzer::new();
        assert_eq!(analyzer.analyze(ast.unwrap()), Err(String::from("Function eq has mismatched types")))
    }

    #[test]
    fn returning_and_passing_fn() {
        let str = "
        fn map(in: Int): Int = in
        fn husky(in: (Int) to Int): (Int) to Int = in
        husky(map)
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        let mut analyzer = StaticAnalyzer::new();
        assert_eq!(analyzer.analyze(ast.unwrap()), Ok(
            Type::Function(
                vec![Type::Integer],
                Box::from(Type::Integer)
            )
        ))
    }

    #[test]
    fn returning_and_passing_fn_wrong() {
        let str = "
        fn map(in: Float): Float = in
        fn husky(in: (Int) to Int): (Int) to Int = in
        husky(map)
        ".to_string();
        let lexer = Lexer::new(&str);
        let tokens = lexer.take_while(|x| x.is_ok())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        let mut analyzer = StaticAnalyzer::new();
        assert_eq!(analyzer.analyze(ast.unwrap()), Err(String::from("Function husky has mismatched types")))
    }

}
