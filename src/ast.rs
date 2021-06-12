use std::fmt::Display;

use crate::token::Location;

#[derive(Debug)]
pub struct Ast {
    pub name: String,
    pub var_decls: Vec<VarDecl>,
    pub func_decls: Vec<FunctionDecl>
}

#[derive(Debug)]
pub struct VarDecl {
    pub prefix: VarPrefix,
    pub name: Id,
    pub data_type: Option<DataTypeLoc>,
    pub init_value: Expr
}

#[derive(Debug)]
pub enum VarPrefix {
    Let(Location),
    Var(Location)
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Id,
    pub params: Vec<FuncParam>,
    pub return_type: DataTypeLoc,
    pub stmt: CmpStmt
}

#[derive(Debug)]
pub struct CmpStmt(pub Vec<Stmt>, pub Location);

#[derive(Debug)]
pub enum Stmt {
    // return an expr or empty
    ReturnStmt(Option<Expr>),

    // local variable declaration
    VarDeclStmt(VarDecl),

    // cond, then-part, optional else-part
    IfElseStmt(Expr, CmpStmt, Option<CmpStmt>),

    // expr
    Expr(Expr)
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    VarRef(Id),
    Assign(Id, Box<Expr>),
    FuncCall(Id, Vec<Expr>)
}

#[derive(Debug)]
pub enum Literal {
    Int(u32, Location),
    Bool(bool, Location)
}

#[derive(Debug)]
pub struct FuncParam {
    pub name: Id,
    pub data_type: DataTypeLoc
}

#[derive(Debug, PartialOrd, PartialEq)]
pub struct Id(pub String, pub Location);

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.0, self.1)
    }
}

#[derive(Debug, PartialOrd, PartialEq)]
pub struct DataTypeLoc(pub DataType, pub Location);

#[derive(Debug, PartialOrd, PartialEq)]
pub enum DataType {
    Int,
    Bool,
    Void
}