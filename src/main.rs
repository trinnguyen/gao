use crate::lexer::Lexer;
use crate::token::Tok;
use crate::token::TokType;

mod lexer;
mod ast;
mod token;
mod parser;

fn main() {
    let path = std::env::args().skip(1).next().unwrap();
    let content = std::fs::read_to_string(path).unwrap();
    let lexer = Lexer::new(&content);

    // debug
    let toks: Vec<Tok> = lexer.collect();
    println!("{:?}", toks);

    let typs: Vec<TokType> = toks.iter().map(|tok| tok.typ.clone()).collect();
    println!("{:?}", typs)
}
