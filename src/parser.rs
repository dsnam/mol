use crate::lexer::Token;
use std::collections::HashMap;

pub enum Expr {
    Binary {
        op: char,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Invoke {
        fn_name: String,
        args: Vec<Expr>,
    },

    Conditional {
        condition: Box<Expr>,
        consequent: Box<Expr>,
        alternative: Box<Expr>,
    },

    For {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Box<Expr>,
    },

    While {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
}

pub struct TypedIdentifier {
    name: String,
    type_name: String,
}

pub struct Prototype {
    pub name: String,
    pub args: Vec<TypedIdentifier>,
    pub out_type: String,
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    operator_precedence: &'a mut HashMap<String, i32>,
}
