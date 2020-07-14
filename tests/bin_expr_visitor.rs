// util for tests

use molva::parser::{Expr, Operator};

pub struct BinExprVisitor {}

impl BinExprVisitor {
    pub fn eval(expr: &Expr) -> f64 {
        dbg!(expr);
        match expr {
            Expr::Binary {
                operator,
                left,
                right,
            } => {
                let left = BinExprVisitor::eval(left);
                let right = BinExprVisitor::eval(right);
                match operator {
                    Operator::Sub => left - right,
                    Operator::Add => left + right,
                    Operator::Mult => left * right,
                    Operator::Div => left / right,
                    Operator::Mod => left % right,
                    _ => panic!("bad operator"),
                }
            }
            Expr::Unary { operator, operand } => {
                let operand = BinExprVisitor::eval(operand);
                if *operator == Operator::Negate {
                    return -operand;
                }
                panic!("couldn't eval")
            }
            Expr::Int(num) => *num as f64,
            Expr::Float(num) => *num,

            _ => panic!("can't handle that expr"),
        }
    }
}
