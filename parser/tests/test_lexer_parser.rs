mod bin_expr_visitor;
use bin_expr_visitor::BinExprVisitor;
use mol_base::ast::Expr::Identifier;
use mol_base::ast::{Expr, Operator};
use mol_parser::mol::{ExprParser, FunctionParser};

#[test]
fn lex_and_parse_fn() {
    let input = "fn do_thing(x: int) -> int =
        let 
            val y = x * 2
        in
            0-44*y";
    let result: Expr = FunctionParser::new().parse(&mut Vec::new(), input).unwrap();
    match result {
        Expr::Function { prototype, body } => assert_eq!(prototype.name, "do_thing"),
        _ => assert!(false),
    }
}

#[test]
fn test_str_parse() {
    let input = "\"test\"";
    let result: Box<Expr> = ExprParser::new().parse(&mut Vec::new(), input).unwrap();
    match *result {
        Expr::StringLiteral(s) => assert_eq!(s, "test"),
        _ => assert!(false),
    }
}

#[test]
fn test_empty_str_parse() {
    let input = "\"\"";
    let result: Box<Expr> = ExprParser::new().parse(&mut Vec::new(), input).unwrap();
    match *result {
        Expr::StringLiteral(s) => assert_eq!(s, ""),
        _ => assert!(false),
    }
}

#[test]
fn test_error_recovery() {
    let input = "
        let
            val b = +a
        in
            a-b
    ";
    let mut errors = Vec::new();
    let result: Box<Expr> = ExprParser::new().parse(&mut errors, input).unwrap();
    match *result {
        Expr::LetIn { val_decls, body } => assert_eq!(
            *body,
            Expr::Binary {
                operator: Operator::Sub,
                left: Box::new(Identifier("a".to_string())),
                right: Box::new(Identifier("b".to_string()))
            }
        ),
        _ => assert!(false),
    }
    assert!(!errors.is_empty())
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
    *ExprParser::new().parse(&mut Vec::new(), input).unwrap()
}

fn eval_expr(input: &str) -> f64 {
    BinExprVisitor::eval(&lex_and_parse_expr(input))
}
