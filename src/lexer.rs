use std::borrow::Borrow;
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;
use Token::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    EOF,
    Fn,
    LeftParen,
    RightParen,
    LeftSqBracket,
    RightSqBracket,
    LeftAngBracket,
    RightAngBracket,
    LeftCurlBracket,
    RightCurlBracket,
    Dot,
    Comma,
    Identifier(String),
    Int(i32),
    Float(f64),
    BoolFalse,
    BoolTrue,
    Comment,
    Op(char),
}

#[derive(Debug)]
pub struct LexerError {
    pub error: String,
}

impl LexerError {
    pub fn new(error_msg: String) -> Self {
        Self { error: error_msg }
    }
}

#[derive(Debug)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
    pub row: usize,
}

impl TokenInfo {
    pub fn new(token: Token, line: usize, row: usize) -> Self {
        Self { token, line, row }
    }
}

pub type LexerResult = Result<TokenInfo, LexerError>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    line: usize,
    row: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            line: 0,
            row: 0,
        }
    }

    fn wrap_token(tok: Token, line: usize, row: usize) -> LexerResult {
        return Ok(TokenInfo::new(tok, line, row));
    }

    fn wrap_error(&mut self, c: char) -> LexerResult {
        let mut msg = String::from("Unconsumable token: ");
        msg.push(c);
        return Err(LexerError::new(String::from(msg)));
    }

    pub fn lex(&mut self) -> LexerResult {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut row = self.row;
        let mut line = self.line;

        loop {
            {
                let ch = chars.peek();
                if ch.is_none() {
                    self.line = line;
                    self.row = row;
                    return Self::wrap_token(EOF, line, row);
                }
                if ch.unwrap().eq('\n'.borrow()) {
                    line += 1;
                    row = 0;
                }
                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }
            chars.next();
            row += 1;
        }
        let start = row;
        let next = chars.next();
        if next.is_none() {
            return Self::wrap_token(EOF, line, row);
        }

        row += 1;

        let result = match next.unwrap() {
            '.' => Self::wrap_token(Dot, line, start),
            '(' => Self::wrap_token(LeftParen, line, start),
            ')' => Self::wrap_token(RightParen, line, start),
            ',' => Self::wrap_token(Comma, line, start),
            '[' => Self::wrap_token(LeftSqBracket, line, start),
            ']' => Self::wrap_token(RightSqBracket, line, start),
            '<' => Self::wrap_token(LeftAngBracket, line, start),
            '>' => Self::wrap_token(RightAngBracket, line, start),
            '{' => Self::wrap_token(LeftCurlBracket, line, start),
            '}' => Self::wrap_token(RightCurlBracket, line, start),
            '0'..='9' => {
                let mut is_float = false;
                loop {
                    let next = match chars.peek() {
                        Some(next) => *next,
                        None => return Self::wrap_token(EOF, line, row),
                    };
                    if !next.is_digit(10) {
                        if next == '.' && !is_float {
                            is_float = true
                        } else {
                            break;
                        }
                    }
                    chars.next();
                    row += 1;
                }
                if is_float {
                    Self::wrap_token(Float(src[start..row].parse().unwrap()), line, start)
                } else {
                    Self::wrap_token(Int(src[start..row].parse().unwrap()), line, start)
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                loop {
                    let next = match chars.peek() {
                        Some(next) => *next,
                        None => return Self::wrap_token(EOF, line, row),
                    };
                    if next != '_' && !next.is_alphanumeric() {
                        break;
                    }
                    chars.next();
                    row += 1;
                }
                let val = match &src[start..row] {
                    "fn" => Self::wrap_token(Fn, line, start),
                    "false" => Self::wrap_token(BoolFalse, line, start),
                    "true" => Self::wrap_token(BoolTrue, line, start),
                    identifier => Self::wrap_token(Identifier(identifier.to_string()), line, start),
                };
                val
            }
            '/' => {
                let next = chars.peek();
                if next == Some(&'/') {
                    loop {
                        let c = chars.next();
                        row += 1;
                        if c == Some('\n') {
                            break;
                        }
                    }
                    Self::wrap_token(Comment, line, start)
                } else {
                    Self::wrap_token(Op('/'), line, start)
                }
            }
            c => self.wrap_error(c),
        };
        self.line = line;
        self.row = row;
        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        let next_res = self.lex();
        match &next_res {
            Ok(info) => match info.token {
                EOF => None,
                _ => Option::from(next_res),
            },
            Err(_) => Option::from(next_res),
        }
    }
}
