use crate::ast::{VarPrefix, Id, Expr};
use crate::core::Location;

pub struct Prog {
    pub name: String,
    pub global_vars: Vec<Variable>,
    pub funcs: Vec<Func>,
}

pub struct Variable {
    pub prefix: VarPrefix,
    pub name: Id,
    pub data_type: Type,
    pub init_value: Expr
}

pub struct Func {
    pub name: Id,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub variables: Vec<Variable>,
    pub blocks: Vec<IrBlock>
}

pub struct IrBlock {
    pub stmts: Vec<IrStmt>
}

pub enum IrStmt {
    Call(IrCallExpr),
    Return(IrExpr)
}

pub struct IrCallExpr {
    pub name: Id,
    pub args: Vec<IrExpr>
}

pub struct IrExpr {

}

pub struct Param {
    pub name: String,
    pub typ: Type
}

pub enum Type {
    Void(Location),
    Int(Location),
    Bool(Location)
}