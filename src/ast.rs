use std::fmt::Display;

use crate::token::Location;

#[derive(Debug)]
pub struct Ast {
    pub var_decls: Vec<VarDecl>,
    pub func_decls: Vec<FunctionDecl>
}

#[derive(Debug)]
pub struct VarDecl {
    pub prefix: VarPrefix,
    pub name: Id,
    pub data_type: Option<DataType>,
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
    pub return_type: FunctionType,
    pub stmt: CmpStmt
}

#[derive(Debug)]
pub struct CmpStmt(pub Vec<Stmt>, pub Location);

#[derive(Debug)]
pub enum Stmt {
    FuncCallStmt(FuncCallExpr),
    ReturnStmt(Option<Expr>),
    VarDeclStmt(VarDecl),
    AssignStmt(Id, Expr)
    // IfElseStmt(Expr, CmpStmt, Option<CmpStmt>)
}

#[derive(Debug)]
pub enum Expr {
    LiteralExpr(Literal),
    VarRefExpr(Id),
    FuncCall(FuncCallExpr)
}

#[derive(Debug)]
pub struct FuncCallExpr {
    pub name: Id,
    pub args: Vec<Expr>
}

#[derive(Debug)]
pub enum Literal {
    Int(u32, Location),
    Bool(bool, Location)
}

#[derive(Debug)]
pub struct FuncParam {
    pub name: Id,
    pub data_type: DataType
}

#[derive(Debug)]
pub enum DataType {
    Int(Location),
    Bool(Location)
}

#[derive(Debug)]
pub struct Id(pub String, pub Location);

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.0, self.1)
    }
}

#[derive(Debug)]
pub enum FunctionType {
    Void,
    Data(DataType)
}