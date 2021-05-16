use check::infer::{TyContext, Typer};
use mol_base::ast::Expr;
use mol_base::types::MolType;
use mol_parser::mol::{ExprParser, FunctionParser};
use std::collections::HashMap;
use std::mem::discriminant;

#[test]
fn test_func_inf() {
    let input = "fn test(a: int) -> int = a+2";
    let res: Expr = FunctionParser::new().parse(&mut Vec::new(), input).unwrap();
    println!("{:?}", res);
    let top_ctx = TyContext::new();
    let inf = Typer::new().infer(top_ctx, res);
    assert_eq!(discriminant(&*inf.ty), discriminant(&MolType::Int))
}

#[test]
fn test_simple_inf() {
    let input = "2";
    let res: Expr = *ExprParser::new().parse(&mut Vec::new(), input).unwrap();
    println!("{:?}", res);
    let top_ctx = TyContext::new();
    let inf = Typer::new().infer(top_ctx, res);
    assert_eq!(discriminant(&*inf.ty), discriminant(&MolType::Int))
}
