extern crate nom;

use nom::*;
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
}

named!(pub lex_fn<&str, Token>, map!(tag!("fn"), |_| FnDef));
//named!(pub lex_identifier<&str, Token>, map!(alpha1, |s| Identifier(s.to_owned())));
