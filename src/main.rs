use log::{debug,info};

use std::path::{Path, PathBuf};

use inkwell::context::Context;
use parser::Parser;

use crate::{gen::LlvmGen, lexer::Lexer};
use clap::{App, Arg};
use inkwell::targets::{TargetMachine, Target, InitializationConfig, RelocMode, CodeModel, FileType};
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use std::process::Command;

mod lexer;
mod ast;
mod token;
mod parser;
mod gen;
mod jit;
mod semantic;
mod ir;
mod core;

fn main() {
    env_logger::init();

    // cli
    let app = App::new("gaoc")
        .author("Tri Nguyen")
        .about("The Gao programming language")
        .arg(Arg::new("run").short('r').long("run").takes_value(false).about("Run JIT compiler"))
        .arg(Arg::new("dump").short('d').long("dump").takes_value(false).about("Dump intermediate files for debugging"))
        .arg(Arg::new("file").required(true));
    let cli_matches = &app.get_matches();
    let should_dump = cli_matches.is_present("dump");

    // read file
    let file = cli_matches.value_of("file").unwrap();
    let path = Path::new(&file);
    let name= path.to_str().unwrap().to_string();
    let content = std::fs::read_to_string(path).unwrap();

    // start lexer
    let lexer = Lexer::new(&content);

    // parse
    let mut parser = Parser::new(lexer);
    let ast = parser.parse(name);
    if should_dump {
        write_file(&serde_yaml::to_string(&ast).unwrap(), path, "yml");
    }

    // gen
    let context = Context::create();
    let mut gen = LlvmGen::new(&ast, &context);
    let module = gen.build();

    // lli
    if should_dump {
        let np = path.with_extension("ll");
        if let Ok(_) = module.print_to_file(&np) {
            debug!("wrote to file {:?}", &np);
        } else {
            debug!("failed to write to file {:?}", &np);
        }
    }

    // run jit
    if cli_matches.is_present("run") {
        run_jit(module);
        return;
    }

    compile_to_obj(module, path);
}

fn run_jit(module: &Module) {
    info!("Invoke JIT");
    // check if having main func
    if module.get_function("main").is_some() {
        let res = jit::execute_jit(module);
        debug!("Exit code: {}", res);
        std::process::exit(res);
    } else {
        debug!("No main found, ignored JIT run")
    }
}

fn compile_to_obj(module: &Module, path: &Path) -> PathBuf {

    // compile to object file
    let triple = TargetMachine::get_default_triple();
    info!("Tripple: {:?}", triple);
    module.set_triple(&triple);

    // init
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let target = Target::from_triple(&triple).unwrap();
    info!("Target: {:?}", target);
    let tm = target.create_target_machine(
        &triple,
        TargetMachine::get_host_cpu_name().to_string().as_str(),
        TargetMachine::get_host_cpu_features().to_string().as_str(),
        OptimizationLevel::None,
        RelocMode::Default,
        CodeModel::Default
    ).unwrap();
    module.set_data_layout(&tm.get_target_data().get_data_layout());

    // write asm
    let pasm = path.with_extension("s");
    tm.write_to_file(module, FileType::Assembly, &pasm).unwrap();
    debug!("wrote to file {:?}", &pasm);

    // write obj
    let pobj = path.with_extension("o");
    tm.write_to_file(module, FileType::Object, &pobj).unwrap();
    debug!("wrote to file {:?}", &pobj);
    pobj
}

fn write_file(content: &str, path: &Path, ext: &str) {
    let new_path = path.with_extension(ext);
    std::fs::write(&new_path, content).unwrap();
    debug!("wrote to file {:?}", &new_path);
}
