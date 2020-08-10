mod bin_expr_visitor;
use bin_expr_visitor::BinExprVisitor;
use mol_base::ast::Expr;
use mol_parser::mol::{ExprParser, FunctionParser};

#[test]
fn lex_and_parse_fn() {
    let input = "fn negate(x: int) -> int =
        let 
            val y = x * 2
        in
            0-44*y";
    let result = FunctionParser::new().parse(input).unwrap();
    assert_eq!(result.prototype.name, "negate");
}

#[test]
fn test_str_parse() {
    let input = "\"test\"";
    let result = ExprParser::new().parse(input).unwrap();
    match *result {
        Expr::StringLiteral(s) => assert_eq!(s, "test"),
        _ => assert!(false),
    }
}

#[test]
fn test_empty_str_parse() {
    let input = "\"\"";
    let result = ExprParser::new().parse(input).unwrap();
    match *result {
        Expr::StringLiteral(s) => assert_eq!(s, ""),
        _ => assert!(false),
    }
}

#[test]
fn test_bin_exprs() {
    assert_eq!(eval_expr("10+5"), 15.0);
    assert_eq!(eval_expr("30-4+22"), 48.0);
    assert_eq!(eval_expr("-1"), -1.0);
    assert_eq!(eval_expr("10%5"), 0.0);
    assert_eq!(eval_expr("(1+2)*4"), 12.0);
    assert_eq!(eval_expr("5--3"), 8.0);
    assert_eq!(eval_expr("5.5 + -3.1 * 4"), -6.9);
}

fn lex_and_parse_expr(input: &str) -> Expr {
    *ExprParser::new().parse(input).unwrap()
}

fn eval_expr(input: &str) -> f64 {
    BinExprVisitor::eval(&lex_and_parse_expr(input))
}
