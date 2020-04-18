use std::iter::Peekable;
use std::str::Chars;

use crate::language::tokenizer::token::{TokenKind, Token, RESERVED_KEYWORDS};

macro_rules! push_while {
    ($lex: ident, $target: ident, $cond: expr) => {{
        while $lex.chars.peek().map_or(false, $cond) {
            $lex.current_char += 1;
            $target.push($lex.chars.next().unwrap());
        }
    }}
}

macro_rules! get_or_propagate {
    ($result: expr) => {{
        match $result {
            Ok(data) => data,
            Err(err) => return Err(err)
        }
    }}
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    current_line: u32,
    current_char: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a String) -> Self {
        Self {
            chars: input.chars().peekable(),
            current_line: 0,
            current_char: 0,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, String>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.peek() {
            None => None,
            Some(char) =>match char {
                '+'  | '-' | '0' ..= '9' => Some(self.next_number()),
                '\"'                     => Some(self.next_string_literal()),
                '\''                     => Some(self.next_char_literal()),
                '('                      => Some(self.skip_construct(TokenKind::LParen)),
                ')'                      => Some(self.skip_construct(TokenKind::RParen)),
                ':'                      => Some(self.skip_construct(TokenKind::Colon)),
                ','                      => Some(self.skip_construct(TokenKind::Coma)),
                ' ' | '\t' | '\n'        => self.skip_blank(),
                _                        => Some(self.next_id())
            }
        }
    }
}

impl Lexer<'_> {

    fn construct_token(&self, t_type: TokenKind) -> Result<Token, String> {
        Ok(Token::new(t_type, self.current_line, self.current_char))
    }

    fn skip_construct(&mut self, t_type: TokenKind) -> Result<Token, String> {
        self.chars.next();
        self.current_char += 1;
        Ok(Token::new(t_type, self.current_line, self.current_char))
    }

    pub fn next_number(&mut self) -> Result<Token, String> {
        let mut number = self.chars.next().unwrap().to_string();
        self.current_char += 1;
        push_while!(self, number, |c| c.is_numeric());
        match self.chars.peek() {
            Some('.') => {
                self.chars.next();
                number.push('.');
                push_while!(self, number, |c| c.is_numeric());
                self.construct_token(TokenKind::FloatLiteral(number.parse().unwrap()))
            },
            _ => self.construct_token(TokenKind::IntegerLiteral(number.parse().unwrap()))
        }
    }

    pub fn next_string_literal(&mut self) -> Result<Token, String> {
        self.chars.next();
        self.current_char += 1;
        let mut is_escaped = false;
        let mut string = "".to_string();
        while self.chars.peek().map_or(false, |c| (is_escaped || c.clone() != '"')) {
           if is_escaped {
               string.push(get_or_propagate!(self.escape_next()))
           } else {
               match self.chars.next() {
                   Some('\'') => is_escaped = true,
                   Some(char) =>string.push(char),
                   None => return Err(format!("Expected closing \" at {}:{} ",
                                              self.current_line, self.current_char))
               }
           }
            self.current_char += 1
        }
        self.current_char += 1;
        match self.chars.next() {
            Some('\"') => self.construct_token(TokenKind::StringLiteral(string)),
            _          => Err(format!("Expected closing \" at {}:{} ",
                                      self.current_line, self.current_char))
        }
    }

    pub fn next_char_literal(&mut self) -> Result<Token, String> {
        self.chars.next();
        self.current_char += 1;
        let char = match self.chars.next() {
            Some('\\')       => get_or_propagate!(self.escape_next()),
            Some(char) => char,
            None             => return Err(format!("Expected character at {}:{} but EOF reached",
                                                   self.current_line, self.current_char))
        };
        self.current_char += 1;
        match self.chars.next() {
            Some('\'') => self.construct_token(TokenKind::CharLiteral(char)),
            _          => Err(format!("Expected closing \' at {}:{} ",
                                      self.current_line, self.current_char))
        }
    }

    pub fn escape_next(&mut self) -> Result<char, String> {
        match self.chars.next() {
            Some('\'') => Ok('\''),
            Some('"')  => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n')  => Ok('\n'),
            Some('r')  => Ok('\r'),
            Some('t')  => Ok('\t'),
            Some('0')  => Ok('\0'),
            Some(unexpected) => Err(format!("Lex error: unexpected escape {} at {}:{}",
                                            unexpected, self.current_line, self.current_char)),
            None                   => Err(format!("Expected character at {}:{} but EOF reached",
                                            self.current_line, self.current_char))
        }
    }

    pub fn skip_blank(&mut self) -> Option<Result<Token, String>> {
        if self.chars.next().unwrap() == '\n' {
            self.current_char = 0;
            self.current_line += 1;
        } else {
            self.current_char += 1;
        }
        self.next()
    }

    pub fn next_id(&mut self) -> Result<Token, String> {
        let mut identifier = "".to_string();
        push_while!(self, identifier, |c| !c.is_whitespace()
                                          && c.clone() != ','
                                          && c.clone() != ')'
                                          && c.clone() != '('
                                          && c.clone() != ':');
        match RESERVED_KEYWORDS.get(identifier.as_str()) {
            Some(kind) => self.construct_token(kind.clone()),
            None => self.construct_token(TokenKind::Id(identifier))
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

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
        let input = "fn 44.33 -32 'b' \"esketit\" () \n , true false let =".to_string();
        let mut lexer = Lexer::new(&input);

        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Fn, 0, 2))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::FloatLiteral(44.33), 0, 7))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::IntegerLiteral(-32), 0, 11))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::CharLiteral('b'), 0, 14))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::StringLiteral("esketit".to_string()), 0, 24))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::LParen, 0, 26))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::RParen, 0, 27))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Coma, 1, 2))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::BooleanLiteral(true), 1, 7))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::BooleanLiteral(false), 1, 13))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Let, 1, 17))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(TokenKind::Assign, 1, 19))));
    }

}
