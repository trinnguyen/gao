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
    ExprStmt(Expr),
    ReturnStmt(Option<Expr>)
}

#[derive(Debug)]
pub enum Expr {
    LiteralExpr(Literal),
    VarRefExpr(Id),
    FuncCallExpr(Id, Vec<Expr>)
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

#[derive(Debug)]
pub enum FunctionType {
    Void,
    Data(DataType)
}