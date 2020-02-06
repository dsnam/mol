use std::borrow::{Borrow, BorrowMut};
use std::io::{self, Write};
use std::iter::Peekable;
use std::ops::{Deref, DerefMut};
use std::str::Chars;
use Token::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    EOF,
    FnDef,
    LeftParen,
    RightParen,
    LeftSqBracket,
    RightSqBracket,
    Comma,
    Identifier(String),
    Int(i32),
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
    pub line: i32,
    pub row: i32,
}

impl TokenInfo {
    pub fn new(token: Token, line: i32, row: i32) -> Self {
        Self { token, line, row }
    }
}

pub type LexerResult = Result<TokenInfo, LexerError>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    line: i32,
    row: i32,
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

    fn wrap_token(&mut self, tok: Token) -> LexerResult {
        return Ok(TokenInfo::new(tok, self.line, self.row));
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
                    return self.wrap_token(EOF);
                }
                if ch.unwrap().eq('\n'.borrow()) {
                    line += 1;
                    row = -1;
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
            return self.wrap_token(EOF);
        }

        let result = match next.unwrap() {
            '(' => self.wrap_token(LeftParen),
            ')' => self.wrap_token(RightParen),
            ',' => self.wrap_token(Comma),
            '[' => self.wrap_token(LeftSqBracket),
            ']' => self.wrap_token(RightSqBracket),
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
                    self.wrap_token(Comment)
                } else {
                    self.wrap_token(Op('/'))
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
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Err(_) => None,
            Ok(info) => match info.token {
                EOF => None,
                tok => Some(tok),
            },
        }
    }
}
