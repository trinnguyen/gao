#[derive(Debug)]
pub struct Ast {
    var_decls: Vec<VarDecl>,
    func_decls: Vec<FunctionDecl>
}

#[derive(Debug)]
pub struct VarDecl {
    prefix: VarPrefix,
    name: String,
    data_type: DataType
}

#[derive(Debug)]
pub enum VarPrefix {
    Let,
    Var
}

#[derive(Debug)]
pub struct FunctionDecl {
    name: String,
    params: Vec<FuncParam>,
    return_type: FunctionType,
    stmt: CmpStmt
}

#[derive(Debug)]
pub struct CmpStmt {
    stmts: Vec<Stmt>
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    ReturnStmt(Option<Expr>)
}

#[derive(Debug)]
pub enum Expr {
    LiteralExpr(Literal),
    VarRefExpr(Box<VarDecl>),
    ParamRefExpr(Box<FuncParam>),
    FuncCallExpr
}

#[derive(Debug)]
pub enum Literal {
    Int(i32),
    Bool(bool)
}

#[derive(Debug)]
pub struct FuncParam {
    name: String,
    data_type: DataType
}

#[derive(Debug)]
pub enum DataType {
    Int,
    Bool
}

#[derive(Debug)]
pub enum FunctionType {
    Void,
    Data(DataType)
}