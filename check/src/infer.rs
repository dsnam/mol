use mol_base::ast::{Expr, TypeDesc};
use mol_base::types::MolType;
use std::collections::HashMap;
use std::mem::discriminant;

pub struct Typer {}

pub struct Inference {
    pub ty: Box<MolType>,
    substitutions: HashMap<String, MolType>,
}

#[derive(Debug, Clone)]
pub struct TyContext {
    env: HashMap<String, MolType>,
    idx: i32,
}

impl TyContext {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            idx: 0,
        }
    }
    pub fn next_var(&mut self) -> String {
        let next_ty = format!("t_{}", self.idx);
        self.idx = self.idx + 1;
        next_ty
    }
}

impl Typer {
    pub fn new() -> Self {
        Self {}
    }
    pub fn infer(&mut self, mut context: TyContext, expr: Expr) -> Inference {
        let mut substitutions = HashMap::new();
        let ty = match expr {
            Expr::Float(_) => MolType::Float,
            Expr::Int(_) => MolType::Int,
            Expr::StringLiteral(_) => MolType::Str,
            Expr::BoolTrue | Expr::BoolFalse => MolType::Bool,
            Expr::ValDec {
                name,
                type_desc,
                value,
            } => {
                // let inf = self.infer(context, *value);
                // context.env.insert(name, *inf.ty);
                // *inf.ty
                unimplemented!()
            }
            Expr::Identifier(s) => context.env.get(&s).unwrap().clone(),
            Expr::Conditional {
                condition,
                consequent,
                alternative,
            } => {
                // todo: infer both and unify
                let inf = self.infer(context, *consequent);
                *inf.ty
            }
            Expr::Application { left, right } => unimplemented!(),
            Expr::Function { prototype, body } => {
                let param_vars = prototype
                    .args
                    .iter()
                    .map(|ident| (ident.name.clone(), Typer::type_from_desc(&ident.type_desc)))
                    .collect();
                let func_context = Typer::create_ctx(&context, param_vars);
                let body_inf = self.infer(func_context, *body);
                substitutions.extend(body_inf.substitutions);
                *body_inf.ty
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                // todo
                let inf = self.infer(context, *left);
                *inf.ty
            }
            _ => unimplemented!(),
        };

        Inference {
            ty: Box::new(ty),
            substitutions,
        }
    }

    // replace any instances of type variables in ty with the substitutions present in the map, eg
    // applying { "a": Int } to Func { args: [ Var("a") ], return_type: Bool } yields
    // Func { args: [ Int ], return_type: Bool }
    fn substitute(substitution: &HashMap<String, MolType>, ty: &MolType) -> MolType {
        match &ty {
            MolType::Named(_) => ty.clone(),
            MolType::Var(s) => match substitution.get(s) {
                Some(sub_ty) => sub_ty.clone(),
                None => ty.clone(),
            },
            MolType::Func { args, return_type } => MolType::Func {
                args: args
                    .iter()
                    .map(|t| Typer::substitute(substitution, t))
                    .collect(),
                return_type: Box::new(Typer::substitute(substitution, return_type)),
            },
            _ => panic!("type not valid for substitution"),
        }
    }

    fn merge_substitutions(
        s1: HashMap<String, MolType>,
        mut s2: HashMap<String, MolType>,
    ) -> HashMap<String, MolType> {
        let mut merged = HashMap::new();
        s2.drain().for_each(|(name, ty)| {
            merged.insert(name, Typer::substitute(&s1, &ty));
        });
        merged
    }

    fn unify(&mut self, ty_1: &MolType, ty_2: &MolType) -> HashMap<String, MolType> {
        let mut substitution = HashMap::new();
        if let MolType::Var(s) = ty_1 {
            substitution.insert(s.clone(), ty_2.clone());
        } else if let MolType::Var(s) = ty_2 {
            substitution.insert(s.clone(), ty_1.clone());
        } else if let MolType::Func {
            args: args_1,
            return_type: return_type_1,
        } = ty_1
        {
            if let MolType::Func {
                args: args_2,
                return_type: return_type_2,
            } = ty_2
            {
                if args_1.len() == args_2.len() {
                    let mut args_sub = HashMap::new();
                    args_1.iter().zip(args_2.iter()).for_each(|(a_1, a_2)| {
                        args_sub.extend(self.unify(a_1, a_2));
                    });
                    let ret_sub = self.unify(
                        &Typer::substitute(&args_sub, return_type_1),
                        &Typer::substitute(&args_sub, return_type_2),
                    );
                    substitution = Typer::merge_substitutions(args_sub, ret_sub);
                }
            }
        } else if discriminant(ty_1) != discriminant(ty_2) {
            panic!("hello i am lazy and panic on type mismatch")
        }
        substitution
    }

    fn create_ctx(context: &TyContext, mut vars: Vec<(String, MolType)>) -> TyContext {
        let mut new_context = context.clone();
        vars.drain(0..).for_each(|(name, ty)| {
            new_context.env.insert(name, ty);
        });
        new_context
    }

    fn type_from_desc(type_desc: &TypeDesc) -> MolType {
        match &type_desc {
            TypeDesc::IntType => MolType::Int,
            TypeDesc::FloatType => MolType::Float,
            TypeDesc::StrType => MolType::Str,
            TypeDesc::BoolType => MolType::Bool,
            TypeDesc::TypeName(s) => MolType::Named(String::from(s)),
            TypeDesc::FnSignature { args, return_type } => MolType::Func {
                args: args.iter().map(|arg| Typer::type_from_desc(arg)).collect(),
                return_type: Box::from(Typer::type_from_desc(return_type)),
            },
        }
    }
}
