use std::borrow::Borrow;
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;
use Token::*;

#[derive(Debug, PartialEq, Clone)]
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
    Colon,
    Semicolon,
    Equal,
    Bang,
    QuestionMark,
    Asterisk,
    Slash,
    Caret, // ^
    Pipe,  // |
    Plus,
    Minus,
    Identifier(String),
    Int(i32),
    Float(f64),
    BoolFalse,
    BoolTrue,
    Comment,
    Op,
    StringLiteral(String),
    If,
    While,
    For,
    Else,
    Return,
    Is,
    As,
    Mut,
    Const,
    // primitives
    IntType,
    FloatType,
    BoolType,
}

#[derive(Debug)]
pub struct LexerError {
    pub error: String,
    pub line: usize,
    pub col: usize,
}

impl LexerError {
    pub fn new(error_msg: String, line: usize, col: usize) -> Self {
        Self {
            error: error_msg,
            line,
            col,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
    pub col: usize,
}

impl TokenInfo {
    pub fn new(token: Token, line: usize, col: usize) -> Self {
        Self { token, line, col }
    }
}

pub type LexerResult = Result<TokenInfo, LexerError>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            line: 0,
            col: 0,
        }
    }

    fn wrap_token(tok: Token, line: usize, col: usize) -> LexerResult {
        return Ok(TokenInfo::new(tok, line, col));
    }

    fn wrap_error(msg: String, line: usize, col: usize) -> LexerResult {
        return Err(LexerError::new(msg, line, col));
    }

    pub fn lex(&mut self) -> LexerResult {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut col = self.col;
        let mut line = self.line;

        loop {
            {
                let ch = chars.peek();
                if ch.is_none() {
                    self.line = line;
                    self.col = col;
                    return Self::wrap_token(EOF, line, col);
                }
                if ch.unwrap().eq('\n'.borrow()) {
                    line += 1;
                    col = 0;
                }
                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }
            chars.next();
            col += 1;
        }
        let start = col;
        let next = chars.next();
        if next.is_none() {
            return Self::wrap_token(EOF, line, col);
        }

        col += 1;

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
            ':' => Self::wrap_token(Colon, line, start),
            ';' => Self::wrap_token(Semicolon, line, start),
            '+' => Self::wrap_token(Plus, line, start),
            '-' => Self::wrap_token(Minus, line, start),
            '!' => Self::wrap_token(Bang, line, start),
            '=' => Self::wrap_token(Equal, line, start),
            '?' => Self::wrap_token(QuestionMark, line, start),
            '*' => Self::wrap_token(Asterisk, line, start),
            '|' => Self::wrap_token(Pipe, line, start),
            '^' => Self::wrap_token(Caret, line, start),
            '0'..='9' => {
                let mut is_float = false;
                loop {
                    let next = match chars.peek() {
                        Some(next) => *next,
                        None => return Self::wrap_token(EOF, line, col),
                    };
                    if !next.is_digit(10) {
                        if next == '.' && !is_float {
                            is_float = true
                        } else {
                            break;
                        }
                    }
                    chars.next();
                    col += 1;
                }
                if is_float {
                    Self::wrap_token(Float(src[start..col].parse().unwrap()), line, start)
                } else {
                    Self::wrap_token(Int(src[start..col].parse().unwrap()), line, start)
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                loop {
                    let next = match chars.peek() {
                        Some(next) => *next,
                        None => return Self::wrap_error(String::from("Unexpected EOF"), line, col),
                    };
                    if next != '_' && !next.is_alphanumeric() {
                        break;
                    }
                    chars.next();
                    col += 1;
                }
                let val = match &src[start..col] {
                    "fn" => Self::wrap_token(Fn, line, start),
                    "false" => Self::wrap_token(BoolFalse, line, start),
                    "true" => Self::wrap_token(BoolTrue, line, start),
                    "if" => Self::wrap_token(If, line, start),
                    "while" => Self::wrap_token(While, line, start),
                    "for" => Self::wrap_token(For, line, start),
                    "else" => Self::wrap_token(Else, line, start),
                    "return" => Self::wrap_token(Return, line, start),
                    "op" => Self::wrap_token(Op, line, start),
                    "is" => Self::wrap_token(Is, line, start),
                    "as" => Self::wrap_token(As, line, start),
                    "int" => Self::wrap_token(IntType, line, start),
                    "bool" => Self::wrap_token(BoolType, line, start),
                    "float" => Self::wrap_token(FloatType, line, start),
                    "mut" => Self::wrap_token(Mut, line, start),
                    "const" => Self::wrap_token(Const, line, start),
                    identifier => Self::wrap_token(Identifier(identifier.to_string()), line, start),
                };
                val
            }
            '/' => {
                let next = chars.peek();
                if next == Some(&'/') {
                    loop {
                        let c = chars.next();
                        col += 1;
                        if c == Some('\n') {
                            break;
                        }
                    }
                    Self::wrap_token(Comment, line, start)
                } else {
                    Self::wrap_token(Slash, line, start)
                }
            }
            '"' => {
                loop {
                    let next = chars.next();
                    col += 1;
                    if next.is_none() {
                        return Self::wrap_error(
                            String::from("Unclosed string literal"),
                            line,
                            start,
                        );
                    }
                    if next == Some('"') {
                        break;
                    }
                }
                Self::wrap_token(
                    StringLiteral(src[start + 1..col - 1].parse().unwrap()),
                    line,
                    start,
                )
            }
            c => Self::wrap_error(
                format!(
                    "Could not process input {} at line {}, col {}",
                    c, line, start
                ),
                line,
                start,
            ),
        };
        self.line = line;
        self.col = col;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn unwrap_token(result: LexerResult) -> Token {
        result.unwrap().token
    }

    fn test_tokens(actual: Vec<Token>, expected: Vec<Token>) {
        assert_eq!(actual.len(), expected.len());
        for (actual_tok, expected_tok) in actual.iter().zip(expected.iter()) {
            assert_eq!(actual_tok, expected_tok);
        }
    }

    #[test]
    fn test_single_lexes() {
        let lexer = Lexer::new(". for method_call() if else \"wow\"");
        let expected = vec![
            Dot,
            For,
            Identifier(String::from("method_call")),
            LeftParen,
            RightParen,
            If,
            Else,
            StringLiteral(String::from("wow")),
        ];
        let tokens = lexer.map(|x| unwrap_token(x)).collect::<Vec<_>>();
        test_tokens(tokens, expected);
    }

    #[test]
    fn test_open_string_fails() {
        let mut lexer = Lexer::new("\"");
        let tokens = lexer.by_ref().collect::<Vec<_>>();
        assert!(!tokens.is_empty());
        let result = tokens.get(0).unwrap();
        assert!(result.is_err());
        let err = result.as_ref().unwrap_err();
        assert_eq!(err.error, "Unclosed string literal")
    }

    #[test]
    fn test_string() {
        let lexer = Lexer::new("\"hi I'm a string\"");
        let tokens = lexer.map(|x| unwrap_token(x)).collect::<Vec<_>>();
        assert_eq!(
            tokens.get(0).unwrap(),
            &StringLiteral(String::from("hi I'm a string"))
        )
    }

    #[test]
    fn test_function_def() {
        let lexer = Lexer::new("fn test(param1: Type1, param2: Type2) -> OutputType {\n}");
        let tokens = lexer.map(|x| unwrap_token(x)).collect::<Vec<_>>();
        let expected = vec![
            Fn,
            Identifier(String::from("test")),
            LeftParen,
            Identifier(String::from("param1")),
            Colon,
            Identifier(String::from("Type1")),
            Comma,
            Identifier(String::from("param2")),
            Colon,
            Identifier(String::from("Type2")),
            RightParen,
            Minus,
            RightAngBracket,
            Identifier(String::from("OutputType")),
            LeftCurlBracket,
            RightCurlBracket,
        ];
        test_tokens(tokens, expected)
    }
}
