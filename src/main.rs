use std::path::Path;

use inkwell::context::Context;
use parser::Parser;

use crate::{gen::LlvmGen, lexer::Lexer};

mod lexer;
mod ast;
mod token;
mod parser;
mod gen;

fn main() {
    env_logger::init();
    let file = std::env::args().skip(1).next().unwrap();
    let path = Path::new(&file);
    let content = std::fs::read_to_string(path).unwrap();
    let lexer = Lexer::new(&content);

    // parse
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    println!("{:?}", ast);

    // gen
    let context = Context::create();
    let mut gen = LlvmGen::new(&ast, &context);
    let val = gen.build();

    // lli
    println!("{}", val);
    let ll_path = path.with_extension("ll");
    std::fs::write(&ll_path, val).unwrap();
}
