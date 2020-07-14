use crate::bin_expr_visitor::BinExprVisitor;
use molva::lexer::{Lexer, LexerResult, Token};
use molva::parser::{Expr, Parser, ParserError};

#[test]
fn lex_and_parse_fn() {
    let input = "fn negate(x: int) -> int {
        val y = x * 2;
        0-x+44%y;
    }";
    let lexer = Lexer::new(input);
    let tokens = lexer.map(|x| x.unwrap()).collect::<Vec<_>>();
    let mut parser = Parser::new(tokens);
    let result = parser.parse_function().unwrap();
    assert_eq!(result.prototype.name, "negate");
}

#[test]
fn test_bin_exprs() {
    assert_eq!(eval_expr("10+5;"), 15.0);
    assert_eq!(eval_expr("30-4+22;"), 48.0);
    assert_eq!(eval_expr("-1;"), -1.0);
    assert_eq!(eval_expr("10%5;"), 0.0);
    assert_eq!(eval_expr("(1+2)*4;"), 12.0);
    assert_eq!(eval_expr("5--3;"), 8.0);
    assert_eq!(eval_expr("5.5 + -3.1 * 4;"), -6.9);
}

fn lex_and_parse_expr(input: &str) -> Result<Expr, ParserError> {
    let lexer = Lexer::new(input);
    let tokens = lexer.map(|x| x.unwrap()).collect::<Vec<_>>();
    let mut parser = Parser::new(tokens);
    parser.parse_expr()
}

fn eval_expr(input: &str) -> f64 {
    BinExprVisitor::eval(&lex_and_parse_expr(input).unwrap())
}
