pub mod token;
pub mod lexer;

#[cfg(test)]
mod tests {
    use crate::language::tokenizer::lexer::Lexer;
    use crate::language::tokenizer::token::{Token, TokenKind};

    #[test]
    fn test_integer() {
        let input = "33 33 rest".to_string();
        let mut lexer = Lexer::new(&input);
        let ret = lexer.next_number();
        assert_eq!(ret, Ok(Token::new(TokenKind::IntegerLiteral(33), 0, 2)));
    }

    #[test]
    fn test_float() {
        let input = "33.35 rest".to_string();
        let mut lexer = Lexer::new(&input);
        let ret = lexer.next_number();
        assert_eq!(ret, Ok(Token::new(TokenKind::FloatLiteral(33.35), 0, 4)));
    }

    #[test]
    fn test_char_literal() {
        let input = "\'a\' \'\\'\'".to_string();
        let mut lexer = Lexer::new(&input);

        let ret = lexer.next_char_literal();
        assert_eq!(ret, Ok(Token::new(TokenKind::CharLiteral('a'), 0, 2)));
        lexer.chars.next();
        let ret = lexer.next_char_literal();
        assert_eq!(ret, Ok(Token::new(TokenKind::CharLiteral('\''), 0, 4)));
    }

    #[test]
    fn test_char_string() {
        let input = "\"er lollero\n\"".to_string();
        let mut lexer = Lexer::new(&input);

        let ret = lexer.next_string_literal();
        assert_eq!(ret, Ok(Token::new(TokenKind::StringLiteral("er lollero\n".to_string()), 0, 13)));
    }

    #[test]
    fn full_test() {
        let input = "fn 44.33 -32 'b' \"esketit\" () \n   true false let =".to_string();
        let mut lexer = Lexer::new(&input);

        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Fn, 0, 2))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::FloatLiteral(44.33), 0, 7))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::IntegerLiteral(-32), 0, 11))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::CharLiteral('b'), 0, 14))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::StringLiteral("esketit".to_string()), 0, 24))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::LParen, 0, 26))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::RParen, 0, 27))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::BooleanLiteral(true), 1, 7))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::BooleanLiteral(false), 1, 13))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Let, 1, 17))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Assign, 1, 19))));
    }
}
