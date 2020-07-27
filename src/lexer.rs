use std::iter::Peekable;
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
    Assign,
    Equal,
    Bang,
    BangEqual,
    LessThanEqual,
    GreaterThanEqual,
    QuestionMark,
    Asterisk,
    Slash,
    Mod,
    Caret, // ^
    Pipe,  // |
    Ampersand,
    Plus,
    Minus,
    Arrow,
    Identifier(String),
    Int(i32),
    Float(f64),
    BoolFalse,
    BoolTrue,
    Comment,
    StringLiteral(String),
    If,
    Else,
    Return,
    Is,
    Val,
    IntType,
    FloatType,
    StrType,
    BoolType,
    When,
    Module,
}

#[derive(Debug)]
pub struct LexerError {
    pub error: String,
    pub line: i32,
    pub col: i32,
}

impl LexerError {
    pub fn new(error_msg: impl Into<String>, line: i32, col: i32) -> Self {
        Self {
            error: error_msg.into(),
            line,
            col,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub line: i32,
    pub col: i32,
}

impl TokenInfo {
    pub fn new(token: Token, line: i32, col: i32) -> Self {
        Self { token, line, col }
    }
}

pub type LexerResult = Result<TokenInfo, LexerError>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    line: i32,
    // column for debugging/error messages
    col: i32,
    // actual position in the input
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            line: 0,
            col: 0,
            pos: 0,
        }
    }

    fn wrap_token(&mut self, tok: Token) -> LexerResult {
        Ok(TokenInfo::new(tok, self.line, self.col))
    }

    fn wrap_token_with_pos(&mut self, tok: Token, col: usize) -> LexerResult {
        Ok(TokenInfo::new(tok, self.line, col as i32))
    }

    fn wrap_error(&self, msg: impl Into<String>) -> LexerResult {
        Err(LexerError::new(msg, self.line, self.col))
    }

    fn wrap_error_with_pos(&self, msg: String, col: usize) -> LexerResult {
        Err(LexerError::new(msg, self.line, col as i32))
    }

    fn next_is(&mut self, expected: char) -> bool {
        match self.chars.peek() {
            Some(actual) => *actual == expected,
            _ => false,
        }
    }

    fn advance(&mut self) {
        self.chars.next();
        self.col += 1;
        self.pos += 1;
    }

    pub fn lex(&mut self) -> LexerResult {
        loop {
            {
                match self.chars.peek() {
                    None => {
                        return self.wrap_token(EOF);
                    }
                    Some(c) => {
                        if c.eq(&'\n') {
                            self.line += 1;
                            self.col = -1;
                        }
                        if !c.is_whitespace() {
                            break;
                        }
                    }
                }
            }
            self.advance();
        }
        let start = self.pos;
        let next = self.chars.next();
        if next.is_none() {
            return self.wrap_token(EOF);
        }

        self.col += 1;
        self.pos += 1;

        let result = match next.unwrap() {
            '.' => Dot,
            '(' => LeftParen,
            ')' => RightParen,
            ',' => Comma,
            '[' => LeftSqBracket,
            ']' => RightSqBracket,
            '<' => {
                if self.next_is('=') {
                    self.advance();
                    LessThanEqual
                } else {
                    LeftAngBracket
                }
            }
            '>' => {
                if self.next_is('=') {
                    self.advance();
                    GreaterThanEqual
                } else {
                    RightAngBracket
                }
            }
            '{' => LeftCurlBracket,
            '}' => RightCurlBracket,
            ':' => Colon,
            ';' => Semicolon,
            '+' => Plus,
            '-' => {
                if self.next_is('>') {
                    self.advance();
                    Arrow
                } else {
                    Minus
                }
            }
            '!' => {
                if self.next_is('=') {
                    self.advance();
                    BangEqual
                } else {
                    Bang
                }
            }
            '=' => {
                if self.next_is('=') {
                    self.advance();
                    Equal
                } else {
                    Assign
                }
            }
            '?' => QuestionMark,
            '*' => Asterisk,
            '|' => Pipe,
            '&' => Ampersand,
            '^' => Caret,
            '%' => Mod,
            '0'..='9' => {
                let mut is_float = false;
                loop {
                    let next = match self.chars.peek() {
                        Some(next) => *next,
                        None => return self.wrap_error("Unexpected EOF"),
                    };
                    if !next.is_digit(10) {
                        if next == '.' && !is_float {
                            is_float = true
                        } else {
                            break;
                        }
                    }
                    self.advance();
                }
                if is_float {
                    Float(self.input[start..self.pos].parse().unwrap())
                } else {
                    Int(self.input[start..self.pos].parse().unwrap())
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                loop {
                    let next = match self.chars.peek() {
                        Some(next) => *next,
                        None => return self.wrap_error("Unexpected EOF"),
                    };
                    if next != '_' && !next.is_alphanumeric() {
                        break;
                    }
                    self.advance();
                }
                match &self.input[start..self.pos] {
                    "fn" => Fn,
                    "false" => BoolFalse,
                    "true" => BoolTrue,
                    "if" => If,
                    "else" => Else,
                    "return" => Return,
                    "is" => Is,
                    "val" => Val,
                    "int" => IntType,
                    "bool" => BoolType,
                    "float" => FloatType,
                    "str" => StrType,
                    "when" => When,
                    "module" => Module,
                    identifier => Identifier(identifier.to_string()),
                }
            }
            '/' => {
                let next = self.chars.peek();
                if next == Some(&'/') {
                    loop {
                        let c = self.chars.next();
                        self.col += 1;
                        self.pos += 1;
                        if c == Some('\n') {
                            break;
                        }
                    }
                    Comment
                } else {
                    Slash
                }
            }
            '"' => {
                loop {
                    let next = self.chars.next();
                    self.col += 1;
                    self.pos += 1;
                    if next.is_none() {
                        return self.wrap_error_with_pos(
                            format!(
                                "Unclosed string literal starting at line {}, col {}",
                                self.line, start
                            ),
                            start,
                        );
                    }
                    if next == Some('"') {
                        break;
                    }
                }

                StringLiteral(self.input[start + 1..self.pos - 1].parse().unwrap())
            }
            c => {
                return self.wrap_error_with_pos(
                    format!(
                        "Could not process input {} at line {}, col {}",
                        c, self.line, start
                    ),
                    start,
                )
            }
        };

        self.wrap_token_with_pos(result, start)
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
        for (actual_tok, expected_tok) in actual.iter().zip(expected.iter()) {
            assert_eq!(actual_tok, expected_tok);
        }
    }

    #[test]
    fn test_single_lexes() {
        let lexer = Lexer::new(". method_call() if else \"wow\"");
        let expected = vec![
            Dot,
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
        assert_eq!(
            err.error,
            "Unclosed string literal starting at line 0, col 0"
        )
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
            Arrow,
            Identifier(String::from("OutputType")),
            LeftCurlBracket,
            RightCurlBracket,
        ];
        test_tokens(tokens, expected)
    }

    #[test]
    fn test_exp() {
        let lexer = Lexer::new(
            "fn negate(x: int) -> int {
                return -x;
            }",
        );
        let tokens = lexer.map(|x| unwrap_token(x)).collect::<Vec<_>>();
        let expected = vec![
            Fn,
            Identifier(String::from("negate")),
            LeftParen,
            Identifier(String::from("x")),
            Colon,
            IntType,
            RightParen,
            Arrow,
            IntType,
            LeftCurlBracket,
            Return,
            Minus,
            Identifier(String::from("x")),
            Semicolon,
            RightCurlBracket,
        ];
        test_tokens(tokens, expected)
    }
}
