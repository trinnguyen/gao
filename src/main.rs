use log::{debug};

use std::path::Path;

use inkwell::context::Context;
use parser::Parser;

use crate::{gen::LlvmGen, lexer::Lexer};

mod lexer;
mod ast;
mod token;
mod parser;
mod gen;
mod semantic;
mod ir;
mod core;

fn main() {
    env_logger::init();
    let file = std::env::args().nth(1).unwrap();
    let path = Path::new(&file);
    let name= path.to_str().unwrap().to_string();
    let content = std::fs::read_to_string(path).unwrap();

    // start lexer
    let lexer = Lexer::new(&content);

    // parse
    let mut parser = Parser::new(lexer);
    let ast = parser.parse(name.to_string());
    write_file(&serde_yaml::to_string(&ast).unwrap(), path, "yml");

    // gen
    let context = Context::create();
    let mut gen = LlvmGen::new(&ast, &context);
    let val = gen.build();

    // lli
    write_file(&val, path, "ll");
}

fn write_file(content: &str, path: &Path, ext: &str) {
    let new_path = path.with_extension(ext);
    std::fs::write(&new_path, content).unwrap();
    debug!("wrote to file {:?}", &new_path);
}
