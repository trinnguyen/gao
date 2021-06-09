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
    let path = std::env::args().skip(1).next().unwrap();
    let content = std::fs::read_to_string(path).unwrap();
    let lexer = Lexer::new(&content);

    // parse
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    println!("{:?}", ast);
}
