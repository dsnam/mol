extern crate mol;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use mol::codegen::Compiler;
use mol_base::ast;
use mol_parser::mol::{FunctionParser, ModuleParser};
use std::env;
use std::fs;

// passing "repl" will make a fake sort of repl where you can type functions and get the IR
// passing a path to a file will output the IR for everything in the file
fn main() {
    let args: Vec<String> = env::args().collect();

    let context = Context::create();
    let module = context.create_module("mol");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();
    let arg = match args.get(1) {
        Some(a) => a,
        None => "repl",
    };

    if arg == "repl" {
        loop {
            let mut input = String::new();
            while !input.contains("\n") {
                let next_line = String::new();
                std::io::stdin().read_line(&mut input).unwrap();
                input.push_str(next_line.as_str())
            }
            if input == "quit" {
                break;
            }
            let function = FunctionParser::new().parse(&input).unwrap();
            let functions = vec![function];
            let mol_mod = ast::Module {
                name: "repl".to_string(),
                functions,
            };
            let fnc = Compiler::compile(&context, &builder, &fpm, &module, &mol_mod).unwrap();
            fnc.iter().for_each(|f| {
                f.print_to_stderr();
            });
        }
    } else {
        let input = fs::read_to_string(arg).expect("error reading file");
        let mol_mod = ModuleParser::new().parse(&input).unwrap();
        let fnc = Compiler::compile(&context, &builder, &fpm, &module, &mol_mod).unwrap();
        fnc.iter().for_each(|f| {
            f.print_to_stderr();
        });
    }
}
