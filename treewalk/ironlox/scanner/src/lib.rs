#![feature(is_some_with)]

use error::LoxError;
use phf::phf_map;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TokenType<'a> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(&'a str),
    String(&'a str),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

#[derive(Debug, Serialize)]
pub struct Token<'a> {
    pub r#type: TokenType<'a>,
    pub line: usize,
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    current_line: usize,
    exhausted: bool,
}

static KEYWORDS: phf::Map<&'static str, TokenType<'static>> = phf_map! {
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "for" => TokenType::For,
    "fun" => TokenType::Fun,
    "if" => TokenType::If,
    "nil" => TokenType::Nil,
    "or" => TokenType::Or,
    "print" => TokenType::Print,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
};

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            current_line: 1,
            exhausted: false,
        }
    }

    fn is_at_end(&self) -> bool {
        self.source.is_empty()
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().next().unwrap();
        self.source = &self.source[1..];
        c
    }

    fn mtch(&mut self, c: char) -> bool {
        let result = self.pmtch(c);
        if result {
            self.advance();
        }
        result
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().next()
    }

    fn pmtch(&self, c: char) -> bool {
        self.peek().is_some_and(|&inner| inner == c)
    }

    fn error<T>(&self, details: &str) -> Result<T, LoxError> {
        Err(LoxError::new(self.current_line, details))
    }

    fn string(&mut self) -> Result<TokenType<'a>, LoxError> {
        match self.source.chars().position(|c| c == '"') {
            None => self.error("Unterminated string."),
            Some(idx) => {
                let value = &self.source[..idx];
                let num_newlines = value.chars().filter(|&c| c == '\n').count();
                self.current_line += num_newlines;
                let token = TokenType::String(value);
                self.source = &self.source[idx + 1..];
                Ok(token)
            }
        }
    }

    fn number(&mut self) -> TokenType<'static> {
        let mut idx = self
            .source
            .chars()
            .position(|c: char| !c.is_ascii_digit())
            .unwrap();
        if self.source.chars().nth(idx).is_some_and(|&c| c == '.')
            && self
                .source
                .chars()
                .nth(idx + 1)
                .is_some_and(char::is_ascii_digit)
        {
            idx += self.source[idx + 1..]
                .chars()
                .position(|c: char| !c.is_ascii_digit())
                .unwrap()
                + 1;
        }
        let result = TokenType::Number(self.source[..idx].parse().unwrap());
        self.source = &self.source[idx..];
        result
    }

    fn identifier(&mut self) -> TokenType<'a> {
        let (ident, source) =
            if let Some(idx) = self.source.chars().position(|c| !is_alphanumeric(c)) {
                (
                    self.source.get(..idx).unwrap(),
                    self.source.get(idx..).unwrap(),
                )
            } else {
                (self.source, "")
            };
        self.source = source;
        KEYWORDS
            .get(ident)
            .cloned()
            .unwrap_or(TokenType::Identifier(ident))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, LoxError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        if self.is_at_end() {
            self.exhausted = true;
            return Some(Ok(Token {
                r#type: TokenType::Eof,
                line: self.current_line,
            }));
        }
        let c = self.peek().unwrap();
        let token_type = match c {
            '(' => {
                self.advance();
                TokenType::LeftParen
            }
            ')' => {
                self.advance();
                TokenType::RightParen
            }
            '{' => {
                self.advance();
                TokenType::LeftBrace
            }
            '}' => {
                self.advance();
                TokenType::RightBrace
            }
            ',' => {
                self.advance();
                TokenType::Comma
            }
            '.' => {
                self.advance();
                TokenType::Dot
            }
            '-' => {
                self.advance();
                TokenType::Minus
            }
            '+' => {
                self.advance();
                TokenType::Plus
            }
            ';' => {
                self.advance();
                TokenType::Semicolon
            }
            '*' => {
                self.advance();
                TokenType::Star
            }
            '!' => {
                self.advance();
                if self.mtch('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            '=' => {
                self.advance();
                if self.mtch('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            '<' => {
                self.advance();
                if self.mtch('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            '>' => {
                self.advance();
                if self.mtch('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '/' => {
                self.advance();
                if self.mtch('/') {
                    while !self.pmtch('\n') && !self.is_at_end() {
                        self.advance();
                    }
                    return self.next();
                } else {
                    TokenType::Slash
                }
            }
            ' ' | '\r' | '\t' => {
                self.advance();
                return self.next();
            }
            '\n' => {
                self.advance();
                self.current_line += 1;
                return self.next();
            }
            '"' => {
                self.advance();
                match self.string() {
                    Ok(it) => it,
                    Err(err) => return Some(Err(err)),
                }
            }
            _ => {
                if c.is_ascii_digit() {
                    self.number()
                } else if is_alpha(c) {
                    self.identifier()
                } else {
                    self.advance();
                    return Some(self.error("Unexpected character."));
                }
            }
        };
        Some(Ok(Token {
            r#type: token_type,
            line: self.current_line,
        }))
    }
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
