use inkwell::{builder::Builder, context::Context, module::Module, types::{BasicType, BasicTypeEnum, IntType}, values::{BasicValue, BasicValueEnum, FunctionValue, IntValue}};

use crate::ast::{Ast, DataType, Expr, FunctionDecl, FunctionType, Literal, Stmt, VarDecl};

pub struct LlvmGen<'a> {
    ast: Ast,
    ctx: &'a Context,
    builder: Builder<'a>,
    module: Module<'a>,
}

impl <'a> LlvmGen<'a> {
    pub fn new(ast: Ast, ctx: &'a Context) -> Self {
        let builder = ctx.create_builder();
        let module = ctx.create_module("default");

        LlvmGen {
            ast,
            ctx,
            builder,
            module
        }
    }

    pub fn build(&self) -> String {
        // go through
        for val_decl in &self.ast.var_decls {
            self.gen_var(val_decl);
        }

        for func_decl in &self.ast.func_decls {
            self.gen_func(func_decl);
        }

        // to string
        self.module.print_to_string().to_string()
    }

    fn gen_var(&self, decl: &VarDecl) {

    }

    fn gen_func(&self, decl: &FunctionDecl) -> FunctionValue {
        let func = self.gen_func_proto(decl);
     
        // create basic block
        let block = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(block);

        // body and return value
        self.gen_cmp_stmt(&decl.stmt.0);
        if func.verify(true) {
            return func;
        }

        panic!("error found on func")
    }

    fn gen_func_proto(&self, decl: &FunctionDecl) -> FunctionValue<'a> {

        // param types
        let param_types = decl.params.iter().map(|param| self.gen_data_type(&param.data_type)).collect::<Vec<BasicTypeEnum>>();

        // function type
        let func_type = match &decl.return_type {
            FunctionType::Void => self.ctx.void_type().fn_type(&param_types, false),
            FunctionType::Data(data_type) => {
                let int_type: IntType = match data_type {
                    DataType::Int(_) => self.ctx.i32_type(),
                    DataType::Bool(_) => self.ctx.bool_type(),
                };
                int_type.fn_type(&param_types, false)
            },
        };

        let func = self.module.add_function(&decl.name.0, func_type, Some(inkwell::module::Linkage::External));

        // set param name
        for (i, basic_value) in func.get_param_iter().enumerate() {
            let param = &decl.params[i];
            match param.data_type {
                DataType::Int(_) | DataType::Bool(_) => basic_value.into_int_value().set_name(&param.name.0),
            }
        }

        return func;
    }

    fn gen_cmp_stmt(&self, stmts: &[Stmt]) {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(expr) => {
                    self.gen_expr(expr);
                },
                Stmt::ReturnStmt(opt_expr) => {
                    if let Some(expr) = opt_expr {
                        let val = self.gen_expr(expr);
                        self.builder.build_return(Some(&val));
                    }

                    return;
                },
            }
        }

        panic!("no return found")
    }

    fn gen_expr(&self, expr: &Expr) -> BasicValueEnum<'a> {
        match expr {
            Expr::LiteralExpr(literal) => self.gen_literal(literal).as_basic_value_enum(),
            Expr::VarRefExpr(name) => todo!(),
            Expr::FuncCallExpr(name, args) => todo!()
        }
    }

    fn gen_literal(&self, literal: &Literal) -> IntValue<'a> {
        match literal {
            Literal::Int(value, _) => self.ctx.i32_type().const_int(*value as u64, false),
            Literal::Bool(value, _) => self.ctx.bool_type().const_int(if *value { 1} else {0}, false),
        }
    }

    fn gen_data_type(&self, dt: &DataType) -> BasicTypeEnum<'a> {
        match dt {
            DataType::Int(_) => self.ctx.i32_type().as_basic_type_enum(),
            DataType::Bool(_) => self.ctx.bool_type().as_basic_type_enum(),
        }   
    }
}