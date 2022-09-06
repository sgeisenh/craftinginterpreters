#![feature(is_some_with)]

use std::{error::Error, fmt};

use phf::phf_map;

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub struct Token<'a> {
    pub r#type: TokenType<'a>,
    pub line: usize,
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    current_line: usize,
}

#[derive(Debug)]
pub struct ScannerError {
    line: usize,
    details: String,
}

impl ScannerError {
    fn new(line: usize, msg: impl Into<String>) -> Self {
        Self {
            line,
            details: msg.into(),
        }
    }
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.details)
    }
}

impl Error for ScannerError {
    fn description(&self) -> &str {
        &self.details
    }
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
            tokens: Vec::new(),
            current_line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token<'a>>, ScannerError> {
        while !self.is_at_end() {
            self.scan_token()?;
        }
        Ok(self.tokens)
    }

    fn is_at_end(&self) -> bool {
        self.source.is_empty()
    }

    fn scan_token(&mut self) -> Result<(), ScannerError> {
        let c = self.peek().unwrap();
        match c {
            '(' => self.munch_token(TokenType::LeftParen),
            ')' => self.munch_token(TokenType::RightParen),
            '{' => self.munch_token(TokenType::LeftBrace),
            '}' => self.munch_token(TokenType::RightBrace),
            ',' => self.munch_token(TokenType::Comma),
            '.' => self.munch_token(TokenType::Dot),
            '-' => self.munch_token(TokenType::Minus),
            '+' => self.munch_token(TokenType::Plus),
            ';' => self.munch_token(TokenType::Semicolon),
            '*' => self.munch_token(TokenType::Star),
            '!' => {
                self.advance();
                if self.mtch('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                self.advance();
                if self.mtch('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            }
            '<' => {
                self.advance();
                if self.mtch('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '>' => {
                self.advance();
                if self.mtch('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            '/' => {
                self.advance();
                if self.mtch('/') {
                    while !self.pmtch('\n') && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {
                self.advance();
            }
            '\n' => {
                self.advance();
                self.current_line += 1;
            }
            '"' => {
                self.advance();
                self.string()?;
            }
            _ => {
                if c.is_ascii_digit() {
                    self.number();
                } else if is_alpha(c) {
                    self.identifier();
                } else {
                    return self.error("Unexpected character.");
                }
            }
        }
        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().next().unwrap();
        self.source = &self.source[1..];
        c
    }

    fn add_token(&mut self, token_type: TokenType<'a>) {
        self.tokens.push(Token {
            r#type: token_type,
            line: self.current_line,
        })
    }

    fn munch_token(&mut self, token_type: TokenType<'a>) {
        self.advance();
        self.add_token(token_type);
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

    fn error<T>(&self, details: &str) -> Result<T, ScannerError> {
        Err(ScannerError::new(self.current_line, details))
    }

    fn string(&mut self) -> Result<(), ScannerError> {
        match self.source.chars().position(|c| c == '"') {
            None => self.error("Unterminated string."),
            Some(idx) => {
                let value = &self.source[..idx];
                let num_newlines = value.chars().filter(|&c| c == '\n').count();
                self.current_line += num_newlines;
                let token = TokenType::String(value);
                self.add_token(token);
                self.source = &self.source[idx + 1..];
                Ok(())
            }
        }
    }

    fn number(&mut self) {
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
        self.add_token(TokenType::Number(self.source[..idx].parse().unwrap()));
        self.source = &self.source[idx..];
    }

    fn identifier(&mut self) {
        let (ident, rest) = self
            .source
            .split_once(|c: char| !is_alphanumeric(c))
            .unwrap_or((self.source, ""));
        let token_type = KEYWORDS
            .get(ident)
            .cloned()
            .unwrap_or(TokenType::Identifier(ident));
        self.add_token(token_type);
        self.source = rest;
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
