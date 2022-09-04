use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};
use mol_base::ast as Mol;
use std::collections::HashMap;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value: Option<FunctionValue<'ctx>>,
}

#[derive(Debug)]
pub struct CompilerError {
    pub error: String,
    pub line: i32,
    pub col: i32,
}

impl CompilerError {
    pub fn new(error_msg: String, line: i32, col: i32) -> Self {
        Self {
            error: error_msg,
            line,
            col,
        }
    }
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        fpm: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        mol_module: &Mol::Module,
    ) -> Result<Vec<FunctionValue<'ctx>>, CompilerError> {
        let mut compiler = Compiler {
            context,
            builder,
            fpm,
            module,
            fn_value: None,
            variables: HashMap::new(),
        };
        compiler.compile_module(mol_module)
    }

    fn get_int_predicate(&self, operator: &Mol::Operator) -> Result<IntPredicate, CompilerError> {
        return match operator {
            Mol::Operator::Equal => Ok(IntPredicate::EQ),
            Mol::Operator::NotEqual => Ok(IntPredicate::NE),
            Mol::Operator::GreaterThan => Ok(IntPredicate::SGT),
            Mol::Operator::GreaterThanEqual => Ok(IntPredicate::SGE),
            Mol::Operator::LessThan => Ok(IntPredicate::SLT),
            Mol::Operator::LessThanEqual => Ok(IntPredicate::SLE),
            _ => Err(self.err("bad operator")),
        };
    }

    fn get_float_predicate(
        &self,
        operator: &Mol::Operator,
    ) -> Result<FloatPredicate, CompilerError> {
        return match operator {
            Mol::Operator::Equal => Ok(FloatPredicate::OEQ),
            Mol::Operator::NotEqual => Ok(FloatPredicate::ONE),
            Mol::Operator::GreaterThan => Ok(FloatPredicate::OGT),
            Mol::Operator::GreaterThanEqual => Ok(FloatPredicate::OGE),
            Mol::Operator::LessThan => Ok(FloatPredicate::OLT),
            Mol::Operator::LessThanEqual => Ok(FloatPredicate::OLE),
            _ => Err(self.err("bad operator")),
        };
    }

    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value.unwrap()
    }

    fn err(&self, msg: impl Into<String>) -> CompilerError {
        CompilerError::new(msg.into(), 0, 0)
    }

    fn compile_expr(&mut self, expr: &Mol::Expr) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        match expr {
            Mol::Expr::Int(x) => Ok(self
                .context
                .i32_type()
                .const_int(x as u64, true)
                .as_basic_value_enum()),
            Mol::Expr::Float(x) => Ok(self
                .context
                .f64_type()
                .const_float(&**x)
                .as_basic_value_enum()),
            Mol::Expr::BoolFalse => Ok(self.context.bool_type().const_zero().as_basic_value_enum()),
            Mol::Expr::BoolTrue => Ok(self
                .context
                .bool_type()
                .const_all_ones()
                .as_basic_value_enum()),
            Mol::Expr::StringLiteral(string) => Ok(self
                .context
                .const_string(string.as_bytes(), false)
                .as_basic_value_enum()),
            Mol::Expr::Identifier(val_name) => match self.variables.get(val_name.as_str()) {
                Some(val) => Ok(self
                    .builder
                    .build_load(*val, val_name.as_str())
                    .as_basic_value_enum()),
                None => Err(self.err("could not find val")),
            },
            Mol::Expr::LetIn {
                ref val_decls,
                ref body,
            } => {
                val_decls.iter().for_each(|v| {
                    self.compile_expr(v);
                });

                self.compile_expr(body)
            }
            Mol::Expr::Conditional {
                ref condition,
                ref consequent,
                ref alternative,
            } => {
                let parent = self.fn_value();
                let zero_const = self.context.bool_type().const_zero();

                let cond = self.compile_expr(condition)?;
                let cond = self.builder.build_int_compare(
                    IntPredicate::NE,
                    cond.into_int_value(),
                    zero_const,
                    "ifcond",
                );

                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(consequent)?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expr(alternative)?;
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(then_val.get_type(), "iftmp");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value())
            }

            Mol::Expr::ValDec {
                ref name,
                ref type_desc,
                ref value,
            } => {
                let v = self.compile_expr(value)?;
                let val_name = name.as_str();
                let ty = match type_desc {
                    Some(desc) => self.get_type(desc),
                    None => v.get_type(),
                };
                let alloca = self.create_entry_block_alloca(&val_name, ty);
                self.builder.build_store(alloca, v);
                self.variables.insert(val_name.to_string(), alloca);

                Ok(alloca.as_basic_value_enum())
            }
            Mol::Expr::Unary {
                ref operator,
                ref operand,
            } => match operator {
                Mol::Operator::Negate => {
                    let val = self.compile_expr(operand)?;
                    if val.is_float_value() {
                        return Ok(self
                            .builder
                            .build_float_neg(val.into_float_value(), "tmpng")
                            .as_basic_value_enum());
                    }
                    if val.is_int_value() {
                        return Ok(self
                            .builder
                            .build_int_neg(val.into_int_value(), "tmpng")
                            .as_basic_value_enum());
                    }
                    Err(self.err("invalid type with - operator"))
                }
                Mol::Operator::Not => {
                    let val = self.compile_expr(operand)?;
                    if val.is_int_value() {
                        return Ok(self
                            .builder
                            .build_not(val.into_int_value(), "tmpnot")
                            .as_basic_value_enum());
                    }
                    Err(self.err("invalid type with ! operator"))
                }
                _ => Err(self.err(format!("bad operator in unary node {:?}", operator))),
            },

            Mol::Expr::Binary {
                operator,
                ref left,
                ref right,
            } => {
                let mut left_val = self.compile_expr(left)?;
                let mut right_val = self.compile_expr(right)?;
                let to_float = left_val.is_float_value() || right_val.is_float_value();
                if to_float {
                    if !left_val.is_float_value() {
                        left_val = self
                            .builder
                            .build_signed_int_to_float(
                                left_val.into_int_value(),
                                self.context.f64_type(),
                                "tmpfloat",
                            )
                            .as_basic_value_enum();
                    }
                    if !right_val.is_float_value() {
                        right_val = self
                            .builder
                            .build_signed_int_to_float(
                                right_val.into_int_value(),
                                self.context.f64_type(),
                                "tmpfloat",
                            )
                            .as_basic_value_enum();
                    }
                }
                match operator {
                    Mol::Operator::Add => {
                        if to_float {
                            return Ok(self
                                .builder
                                .build_float_add(
                                    left_val.into_float_value(),
                                    right_val.into_float_value(),
                                    "tmpadd",
                                )
                                .as_basic_value_enum());
                        }
                        return Ok(self
                            .builder
                            .build_int_add(
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "tmpadd",
                            )
                            .as_basic_value_enum());
                    }
                    Mol::Operator::And => Ok(self
                        .builder
                        .build_and(
                            left_val.into_int_value(),
                            right_val.into_int_value(),
                            "tmpand",
                        )
                        .as_basic_value_enum()),
                    Mol::Operator::Div => {
                        if to_float {
                            return Ok(self
                                .builder
                                .build_float_div(
                                    left_val.into_float_value(),
                                    right_val.into_float_value(),
                                    "tmpdiv",
                                )
                                .as_basic_value_enum());
                        }
                        return Ok(self
                            .builder
                            .build_int_signed_div(
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "tmpdiv",
                            )
                            .as_basic_value_enum());
                    }
                    Mol::Operator::Equal
                    | Mol::Operator::NotEqual
                    | Mol::Operator::GreaterThanEqual
                    | Mol::Operator::GreaterThan
                    | Mol::Operator::LessThanEqual
                    | Mol::Operator::LessThan => {
                        if to_float {
                            return Ok(self
                                .builder
                                .build_float_compare(
                                    self.get_float_predicate(operator)?,
                                    left_val.into_float_value(),
                                    right_val.into_float_value(),
                                    "tmpcmp",
                                )
                                .as_basic_value_enum());
                        }
                        return Ok(self
                            .builder
                            .build_int_compare(
                                self.get_int_predicate(operator)?,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "tmpcmp",
                            )
                            .as_basic_value_enum());
                    }
                    // Mol::Operator::Mod => {}
                    Mol::Operator::Mult => {
                        if to_float {
                            return Ok(self
                                .builder
                                .build_float_mul(
                                    left_val.into_float_value(),
                                    right_val.into_float_value(),
                                    "tmpmul",
                                )
                                .as_basic_value_enum());
                        }
                        return Ok(self
                            .builder
                            .build_int_mul(
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "tmpmul",
                            )
                            .as_basic_value_enum());
                    }
                    Mol::Operator::Or => {
                        return Ok(self
                            .builder
                            .build_or(
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "tmpor",
                            )
                            .as_basic_value_enum())
                    }
                    Mol::Operator::Sub => {
                        if to_float {
                            return Ok(self
                                .builder
                                .build_float_sub(
                                    left_val.into_float_value(),
                                    right_val.into_float_value(),
                                    "tmpsub",
                                )
                                .as_basic_value_enum());
                        }
                        return Ok(self
                            .builder
                            .build_int_sub(
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "tmpsub",
                            )
                            .as_basic_value_enum());
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
            // Mol::Expr::Invoke {
            //     ref fn_name,
            //     ref args,
            // } => match self.get_function(fn_name.as_str()) {
            //     Some(fun) => {
            //         let mut compiled_args = Vec::with_capacity(args.len());
            //         for arg in args {
            //             compiled_args.push(self.compile_expr(arg)?);
            //         }
            //
            //         let argsv: Vec<BasicValueEnum> = compiled_args
            //             .iter()
            //             .by_ref()
            //             .map(|&val| val.into())
            //             .collect();
            //
            //         match self
            //             .builder
            //             .build_call(fun, argsv.as_slice(), "tmp")
            //             .try_as_basic_value()
            //             .left()
            //         {
            //             Some(value) => Ok(value.as_basic_value_enum()),
            //             None => Err(self.err("function not found")),
            //         }
            //     }
            //     None => Err(self.err(format!("invalid function {}", fn_name))),
            // },
        }
    }

    fn get_type(&self, type_desc: &Mol::TypeDesc) -> BasicTypeEnum<'ctx> {
        match type_desc {
            Mol::TypeDesc::IntType => BasicTypeEnum::IntType(self.context.i32_type()),
            Mol::TypeDesc::BoolType => BasicTypeEnum::IntType(self.context.bool_type()),
            Mol::TypeDesc::FloatType => BasicTypeEnum::FloatType(self.context.f64_type()),
            _ => unimplemented!(),
        }
    }

    fn make_return(&self, val: BasicValueEnum<'ctx>) {
        match val {
            BasicValueEnum::IntValue(v) => self.builder.build_return(Some(&v)),
            BasicValueEnum::FloatValue(v) => self.builder.build_return(Some(&v)),
            _ => unimplemented!(),
        };
    }

    fn create_entry_block_alloca(&self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name)
    }

    fn compile_module(
        &mut self,
        module: &Mol::Module,
    ) -> Result<Vec<FunctionValue<'ctx>>, CompilerError> {
        // first compile all prototypes
        module.functions.iter().for_each(|func| {
            self.compile_prototype(&func.prototype).unwrap();
        });
        let mut function_values = vec![];
        for func in &module.functions {
            function_values.push(self.compile_fn(func)?);
        }
        Ok(function_values)
    }

    fn compile_prototype(
        &self,
        prototype: &Mol::Prototype,
    ) -> Result<FunctionValue<'ctx>, CompilerError> {
        let return_type = self.get_type(&prototype.return_type);
        let args_types = prototype
            .args
            .iter()
            .map(|t| self.get_type(&t.type_desc))
            .collect::<Vec<BasicMetadataTypeEnum>>()
            .as_slice();

        let fn_type = return_type.fn_type(args_types, false);

        let fn_val = self
            .module
            .add_function(prototype.name.as_str(), fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(prototype.args[i].name.as_str())
        }
        Ok(fn_val)
    }

    fn compile_fn(
        &mut self,
        function: &Mol::Function,
    ) -> Result<FunctionValue<'ctx>, CompilerError> {
        let prototype = &function.prototype;
        let function_value = match self.get_function(&function.prototype.name) {
            Some(fv) => fv,
            None => {
                return Err(self.err(format!(
                    "failed to compile prototype for fn {:?}",
                    &function.prototype.name
                )))
            }
        };
        let entry = self.context.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry);
        self.fn_value = Some(function_value);

        for (i, arg) in function_value.get_param_iter().enumerate() {
            let arg_name = prototype.args[i].name.as_str();
            let alloca = self.create_entry_block_alloca(arg_name, arg.get_type());
            self.builder.build_store(alloca, arg);
            self.variables.insert(arg_name.to_owned(), alloca);
        }

        let return_val = self.compile_expr(&function.body)?;

        self.make_return(return_val);

        if function_value.verify(true) {
            self.fpm.run_on(&function_value);
            Ok(function_value)
        } else {
            unsafe {
                function_value.delete();
            }

            Err(self.err(format!("invalid function: {:?}", &function.prototype.name)))
        }
    }
}
