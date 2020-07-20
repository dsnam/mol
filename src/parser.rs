use crate::lexer::{Token, Token::*, TokenInfo};
use crate::parser::Expr::{Conditional, Invoke, Unary, ValDec, Variable};
use crate::parser::Operator::Not;
use crate::parser::TypeDesc::FnSignature;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum Expr {
    Invoke {
        fn_name: String,
        args: Vec<Expr>,
    },

    Binary {
        operator: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Unary {
        operator: Operator,
        operand: Box<Expr>,
    },

    Conditional {
        condition: Box<Expr>,
        consequent: Box<Expr>,
        alternative: Option<Box<Expr>>,
    },

    ValDec {
        name: String,
        type_name: Option<TypeDesc>,
        value: Box<Expr>,
    },

    Float(f64),
    StringLiteral(String),
    Int(i32),
    BoolTrue,
    BoolFalse,
    Variable(String),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Not,
    And,
    Or,
    Assign,
    Negate,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Deref,
}

#[derive(Debug, PartialEq)]
pub struct TypedIdentifier {
    name: String,
    type_desc: TypeDesc,
}

#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Option<Vec<TypedIdentifier>>,
    pub return_type: TypeDesc,
}

#[derive(Debug, PartialEq)]
pub enum TypeDesc {
    TypeName(String),
    FnSignature {
        args: Option<Vec<TypeDesc>>,
        return_type: Box<TypeDesc>,
    },
    IntType,
    FloatType,
    StrType,
    BoolType,
}

#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub struct Module {
    pub functions: HashMap<String, Function>,
    pub name: String,
}

#[derive(Debug)]
pub struct ParserError {
    pub error: String,
    pub line: i32,
    pub col: i32,
}

impl ParserError {
    pub fn new(error_msg: String, line: i32, col: i32) -> Self {
        Self {
            error: error_msg,
            line,
            col,
        }
    }
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} at line {} col {}", self.error, self.line, self.col)
    }
}

pub struct Parser {
    tokens: Vec<TokenInfo>,
    pos: usize,
}

impl Parser {
    pub fn new(input: Vec<TokenInfo>) -> Self {
        let filtered_tokens = input.into_iter().filter(|t| t.token != Comment).collect();
        Self {
            tokens: filtered_tokens,
            pos: 0,
        }
    }

    fn operator_precedence(operator: Operator) -> i32 {
        match operator {
            Operator::Deref => 12,
            Operator::Not | Operator::Negate => 11,
            Operator::Mult | Operator::Div | Operator::Mod => 10,
            Operator::Add | Operator::Sub => 9,
            Operator::LessThan
            | Operator::LessThanEqual
            | Operator::GreaterThan
            | Operator::GreaterThanEqual => 8,
            Operator::Equal | Operator::NotEqual => 7,
            Operator::And => 6,
            Operator::Or => 5,
            Operator::Assign => 1,
        }
    }

    fn unary_operator_for(token: &Token) -> Option<Operator> {
        match token {
            Bang => Some(Operator::Not),
            Minus => Some(Operator::Negate),
            _ => None,
        }
    }

    fn binary_operator_for(token: &Token) -> Option<Operator> {
        match token {
            Equal => Some(Operator::Equal),
            BangEqual => Some(Operator::NotEqual),
            Asterisk => Some(Operator::Mult),
            Mod => Some(Operator::Mod),
            Ampersand => Some(Operator::And),
            Plus => Some(Operator::Add),
            Minus => Some(Operator::Sub),
            Slash => Some(Operator::Div),
            Dot => Some(Operator::Deref),
            LeftAngBracket => Some(Operator::LessThan),
            RightAngBracket => Some(Operator::GreaterThan),
            LessThanEqual => Some(Operator::LessThanEqual),
            GreaterThanEqual => Some(Operator::GreaterThanEqual),
            _ => None,
        }
    }

    fn current_precedence(&self) -> i32 {
        return match Parser::binary_operator_for(&self.curr()) {
            Some(x) => Parser::operator_precedence(x),
            None => -1,
        };
    }

    fn current(&self) -> Result<TokenInfo, ParserError> {
        if self.pos >= self.tokens.len() {
            Err(ParserError::new(String::from("Unexpected EOF"), 0, 0))
        } else {
            Ok(self.tokens[self.pos].clone())
        }
    }

    fn curr(&self) -> Token {
        self.tokens[self.pos].clone().token
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn advance(&mut self) -> Result<(), ParserError> {
        self.pos += 1;
        if self.pos <= self.tokens.len() {
            Ok(())
        } else {
            Err(self.wrap_error(String::from("Unexpected EOF")))
        }
    }

    fn next(&self) -> Token {
        if self.pos + 1 >= self.tokens.len() {
            return EOF;
        }
        self.tokens[self.pos + 1].clone().token
    }

    fn wrap_error(&self, error_msg: String) -> ParserError {
        match self.current() {
            Ok(tok) => ParserError::new(error_msg, tok.line, tok.col),
            Err(e) => e,
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), ParserError> {
        if self.curr() == token {
            self.advance()?;
            Ok(())
        } else {
            let info = self.current()?;
            Err(self.wrap_error(format!(
                "Expected {:?} found {:?} at line {:?} col {:?}",
                token, info.token, info.line, info.col
            )))
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        let mut functions = HashMap::new();
        let mut name = String::from("anon");
        let mut module_named = false;
        loop {
            if self.at_end() {
                break;
            }
            match self.current()?.token {
                Fn => {
                    let function = self.parse_function()?;
                    functions.insert(function.prototype.name.clone(), function);
                }
                Module => {
                    if !module_named {
                        module_named = true;
                        name = self.parse_module_decl()?;
                    } else {
                        return Err(self
                            .wrap_error(String::from("multiple module name declarations found")));
                    }
                }
                _ => {
                    return Err(self.wrap_error(String::from(
                        "only functions and the module name are allowed at the top level",
                    )))
                }
            };
        }

        Ok(Module { name, functions })
    }

    fn parse_module_decl(&mut self) -> Result<String, ParserError> {
        self.expect(Module)?;

        let module = match self.parse_identifier()? {
            Variable(s) => Ok(s),
            _ => Err(self.wrap_error(String::from("invalid module name"))),
        };

        self.expect(Semicolon)?;

        module
    }

    fn parse_const_decl(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_statement(&mut self) -> Result<Expr, ParserError> {
        match self.curr() {
            Val => self.parse_val_decl(),
            Return => self.parse_return(),
            _ => self.parse_expr_st(),
        }
    }

    fn parse_val_decl(&mut self) -> Result<Expr, ParserError> {
        self.expect(Val)?;
        let name = match self.curr() {
            Identifier(s) => {
                self.advance()?;
                s
            }
            _ => return Err(self.wrap_error(String::from("Expected identifier"))),
        };
        let val_type = match self.parse_type_desc() {
            Ok(type_desc) => Some(type_desc),
            _ => None,
        };

        self.expect(Assign)?;

        let expr = self.parse_expr()?;

        self.expect(Semicolon)?;

        Ok(ValDec {
            name,
            type_name: val_type,
            value: Box::new(expr),
        })
    }

    fn parse_return(&mut self) -> Result<Expr, ParserError> {
        self.expect(Return)?;
        let expr = self.parse_expr()?;
        self.expect(Semicolon)?;
        Ok(expr)
    }

    fn parse_expr_st(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_expr()?;
        self.expect(Semicolon)?;
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        match self.curr() {
            Identifier(_) => self.parse_identifier(),
            LeftParen => self.parse_paren_expr(),
            If => self.parse_conditional(),
            _ => self.parse_literal(),
        }
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(LeftParen)?;

        let expr = self.parse_expr()?;

        self.expect(RightParen)?;

        Ok(expr)
    }

    fn parse_when(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    pub fn parse_function(&mut self) -> Result<Function, ParserError> {
        let prototype = self.parse_prototype()?;
        let body = self.parse_block()?;
        Ok(Function { prototype, body })
    }

    fn parse_fn_args_def(&mut self) -> Result<Option<Vec<TypedIdentifier>>, ParserError> {
        self.expect(LeftParen)?;
        if self.next() == RightParen {
            Ok(None)
        } else {
            let mut args = Vec::new();
            loop {
                args.push(self.parse_typed_identifier()?);
                match self.curr() {
                    RightParen => {
                        self.advance()?;
                        break;
                    }
                    Comma => {
                        self.advance()?;
                    }
                    _ => {
                        return Err(self.wrap_error(String::from("Expected ',' or ')'")));
                    }
                }
            }
            Ok(Some(args))
        }
    }

    fn parse_prototype(&mut self) -> Result<Prototype, ParserError> {
        self.expect(Fn)?;

        let name = match self.curr() {
            Identifier(s) => {
                self.advance()?;
                s
            }
            _ => return Err(self.wrap_error(String::from("Expected identifier"))),
        };

        let args = self.parse_fn_args_def()?;

        let return_type = self.parse_fn_return_type()?;

        Ok(Prototype {
            name,
            args,
            return_type,
        })
    }

    fn parse_block(&mut self) -> Result<Vec<Expr>, ParserError> {
        self.expect(LeftCurlBracket)?;
        let mut statements = vec![];
        while self.curr() != RightCurlBracket {
            statements.push(self.parse_statement()?);
        }
        self.expect(RightCurlBracket)?;
        Ok(statements)
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        match self.parse_unary_expr() {
            Ok(left) => self.parse_binary_expr(0, left),
            err => err,
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParserError> {
        let op = Parser::unary_operator_for(&self.curr());
        match op {
            Some(op) => {
                self.advance()?;
                let operand = self.parse_primary()?;
                Ok(Unary {
                    operator: op,
                    operand: Box::new(operand),
                })
            }
            None => self.parse_primary(),
        }
    }

    fn parse_binary_expr(
        &mut self,
        lhs_precedence: i32,
        mut left: Expr,
    ) -> Result<Expr, ParserError> {
        loop {
            let current_precedence = self.current_precedence();
            if current_precedence < lhs_precedence || self.at_end() {
                return Ok(left);
            }

            let op = match Parser::binary_operator_for(&self.curr()) {
                Some(o) => o,
                None => return Err(self.wrap_error(String::from("Expected operator"))),
            };

            self.advance()?;

            let mut right = self.parse_unary_expr()?;
            let next_precedence = self.current_precedence();
            if current_precedence < next_precedence {
                right = self.parse_binary_expr(current_precedence + 1, right)?;
            }

            left = Expr::Binary {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
    }

    fn parse_conditional(&mut self) -> Result<Expr, ParserError> {
        self.expect(If)?;

        let condition = self.parse_expr()?;
        self.expect(LeftCurlBracket)?;
        let consequent = self.parse_expr()?;
        let alternative = match self.next() {
            Else => {
                self.expect(Else)?;
                if self.next() == If {
                    Some(Box::new(self.parse_conditional()?))
                } else {
                    Some(Box::new(self.parse_expr()?))
                }
            }
            _ => None,
        };

        Ok(Conditional {
            condition: Box::new(condition),
            consequent: Box::new(consequent),
            alternative,
        })
    }

    fn parse_identifier(&mut self) -> Result<Expr, ParserError> {
        let name = match self.curr() {
            Identifier(s) => {
                self.advance()?;
                s
            }
            _ => return Err(self.wrap_error(String::from("Expected identifier"))),
        };

        match self.curr() {
            LeftParen => {
                self.advance()?;
                if let RightParen = self.curr() {
                    return Ok(Invoke {
                        fn_name: name,
                        args: vec![],
                    });
                }

                let mut args = vec![];

                loop {
                    args.push(self.parse_expr()?);

                    match self.curr() {
                        Comma => (),
                        RightParen => break,
                        _ => return Err(self.wrap_error(String::from("expected comma"))),
                    }

                    self.advance()?;
                }
                self.advance()?;

                Ok(Invoke {
                    fn_name: name,
                    args,
                })
            }
            _ => Ok(Variable(name)),
        }
    }

    fn parse_fn_type_desc(&mut self) -> Result<TypeDesc, ParserError> {
        self.expect(LeftParen)?;
        if self.curr() == RightParen {
            self.advance()?;
            let return_type = self.parse_fn_return_type()?;
            return Ok(FnSignature {
                args: None,
                return_type: Box::new(return_type),
            });
        }
        let mut fn_args = Vec::new();
        loop {
            fn_args.push(self.parse_type_desc()?);
            match self.curr() {
                RightParen => {
                    self.advance()?;
                    break;
                }
                Comma => {
                    self.advance()?;
                }
                _ => {
                    return Err(self.wrap_error(String::from("Failed to parse fn type description")))
                }
            }
        }
        let return_type = self.parse_fn_return_type()?;
        Ok(FnSignature {
            args: Some(fn_args),
            return_type: Box::new(return_type),
        })
    }

    fn parse_fn_return_type(&mut self) -> Result<TypeDesc, ParserError> {
        self.expect(Arrow)?;
        self.parse_type_desc()
    }

    fn parse_type_desc(&mut self) -> Result<TypeDesc, ParserError> {
        match self.current()?.token {
            Identifier(s) => {
                self.advance()?;
                Ok(TypeDesc::TypeName(s))
            }
            LeftParen => Ok(self.parse_fn_type_desc()?),
            IntType => {
                self.advance()?;
                Ok(TypeDesc::IntType)
            }
            FloatType => {
                self.advance()?;
                Ok(TypeDesc::FloatType)
            }
            StrType => {
                self.advance()?;
                Ok(TypeDesc::StrType)
            }
            BoolType => {
                self.advance()?;
                Ok(TypeDesc::BoolType)
            }
            _ => Err(self.wrap_error(String::from("Expected type name or signature"))),
        }
    }

    fn parse_typed_identifier(&mut self) -> Result<TypedIdentifier, ParserError> {
        let name = match self.curr() {
            Identifier(s) => {
                self.advance()?;
                s
            }
            _ => return Err(self.wrap_error(String::from("Expected identifier"))),
        };
        self.expect(Colon)?;
        let type_desc = self.parse_type_desc()?;
        Ok(TypedIdentifier { name, type_desc })
    }

    fn parse_literal(&mut self) -> Result<Expr, ParserError> {
        match self.current()?.token {
            Int(i) => {
                self.advance()?;
                Ok(Expr::Int(i))
            }
            Float(f) => {
                self.advance()?;
                Ok(Expr::Float(f))
            }
            StringLiteral(s) => {
                self.advance()?;
                Ok(Expr::StringLiteral(s))
            }
            BoolTrue => {
                self.advance()?;
                Ok(Expr::BoolTrue)
            }
            BoolFalse => {
                self.advance()?;
                Ok(Expr::BoolFalse)
            }
            _ => Err(self.wrap_error(format!("Unexpected {:?}", self.curr()))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::parser::TypeDesc::{FnSignature, TypeName};

    fn wrap_tok(token: Token) -> TokenInfo {
        TokenInfo {
            token,
            line: 0,
            col: 0,
        }
    }

    #[test]
    fn test_parse_fn_proto() {
        let tokens = vec![
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
        ]
        .iter()
        .map(|t| wrap_tok(t.clone()))
        .collect::<Vec<_>>();
        let mut parser = Parser::new(tokens);
        let proto = parser.parse_prototype().unwrap();
        let args = proto.args.unwrap();
        assert_eq!(proto.name, "test");
        assert_eq!(args.len(), 2);
        assert_eq!(args.get(0).unwrap().name, "param1");
        assert_eq!(
            args.get(0).unwrap().type_desc,
            TypeName(String::from("Type1"))
        );
        assert_eq!(proto.return_type, TypeName(String::from("OutputType")));
    }

    #[test]
    fn test_parse_fn_proto_with_fn_arg() {
        // fn test(param1: Type1, param2: (int) -> bool) -> OutputType
        let tokens = vec![
            Fn,
            Identifier(String::from("test")),
            LeftParen,
            Identifier(String::from("param1")),
            Colon,
            Identifier(String::from("Type1")),
            Comma,
            Identifier(String::from("param2")),
            Colon,
            LeftParen,
            IntType,
            RightParen,
            Arrow,
            BoolType,
            RightParen,
            Arrow,
            Identifier(String::from("OutputType")),
        ]
        .iter()
        .map(|t| wrap_tok(t.clone()))
        .collect::<Vec<_>>();
        let mut parser = Parser::new(tokens);
        let proto = parser.parse_prototype().unwrap();
        let args = proto.args.unwrap();
        assert_eq!(proto.name, "test");
        assert_eq!(args.len(), 2);
        assert_eq!(args.get(0).unwrap().name, "param1");
        assert_eq!(
            args.get(0).unwrap().type_desc,
            TypeName(String::from("Type1"))
        );
        assert_eq!(args.get(1).unwrap().name, "param2");
        assert_eq!(
            args.get(1).unwrap().type_desc,
            FnSignature {
                args: Some(vec![TypeDesc::IntType]),
                return_type: Box::new(TypeDesc::BoolType)
            }
        );
        assert_eq!(proto.return_type, TypeName(String::from("OutputType")));
    }

    #[test]
    fn test_parse_full_fn() {
        // fn test(param1: int) -> int {
        //   return param1 + 4;
        // }
        let tokens = vec![
            Fn,
            Identifier(String::from("test")),
            LeftParen,
            Identifier(String::from("param1")),
            Colon,
            IntType,
            RightParen,
            Arrow,
            IntType,
            LeftCurlBracket,
            Return,
            Identifier(String::from("param1")),
            Plus,
            Int(4),
            Semicolon,
            RightCurlBracket,
        ]
        .iter()
        .map(|t| wrap_tok(t.clone()))
        .collect::<Vec<_>>();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_function().unwrap();
        assert_eq!(result.prototype.name, String::from("test"));
    }
}
