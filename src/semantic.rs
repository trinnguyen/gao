use crate::ast::{Ast, FunctionDecl, DataType};

pub struct Validator<'a> {
    ast: &'a Ast
}

impl <'a> Validator<'a> {

    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast
        }
    }

    pub fn validate(&self) {
        // make sure function overloads are distinct
        // TODO

        for decl in &self.ast.func_decls {
            self.check_func(decl)
        }
    }

    fn check_func(&self, func: &FunctionDecl) {

    }

}

struct FuncSignature {
    name: String,
    params: Vec<DataType>
}