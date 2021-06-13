use std::fmt::Display;
use serde::{Serialize, Deserialize};

use crate::core::Location;

#[derive(Debug, Serialize, Deserialize)]
pub struct Ast {
    pub name: String,
    pub var_decls: Vec<VarDecl>,
    pub func_decls: Vec<FunctionDecl>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VarDecl {
    pub prefix: VarPrefix,
    pub name: Id,
    pub data_type: Option<DataTypeLoc>,
    pub init_value: Expr
}

#[derive(Debug, Serialize, Deserialize)]
pub enum VarPrefix {
    Let(Location),
    Var(Location)
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: Id,
    pub params: Vec<FuncParam>,
    pub return_type: DataTypeLoc,
    pub stmt: CmpStmt
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CmpStmt(pub Vec<Stmt>, pub Location);

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
pub enum Expr {
    Literal(Literal),
    VarRef(Id),
    Assign(Id, Box<Expr>),
    FuncCall(Id, Vec<Expr>),
    BinOpExpr(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>)
}

#[derive(Debug, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Sub
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Literal {
    Int(u32, Location),
    Bool(bool, Location)
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FuncParam {
    pub name: Id,
    pub data_type: DataTypeLoc
}

#[derive(Debug, PartialOrd, PartialEq, Serialize, Deserialize)]
pub struct Id(pub String, pub Location);

#[derive(Debug, PartialOrd, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Or,
    And,
    Eq,
    Neq,
    Gt,
    Lt,
    Ge,
    Le,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, PartialOrd, PartialEq, Serialize, Deserialize)]
pub struct DataTypeLoc(pub DataType, pub Location);

#[derive(Debug, PartialOrd, PartialEq, Serialize, Deserialize)]
pub enum DataType {
    Int,
    Bool,
    Void
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.0, self.1)
    }
}