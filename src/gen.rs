use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, IntType},
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::ast::{
    Ast, DataType, Expr, FuncCallExpr, FunctionDecl, FunctionType, Literal, Stmt, VarDecl,
};

pub struct LlvmGen<'a> {
    ast: &'a Ast,
    ctx: &'a Context,
    builder: Builder<'a>,
    module: Module<'a>,
    var_table: HashMap<String, PointerValue<'a>>,
    func_table: HashMap<String, FunctionValue<'a>>,
}

impl<'a> LlvmGen<'a> {
    pub fn new(ast: &'a Ast, ctx: &'a Context) -> Self {
        let builder = ctx.create_builder();
        let module = ctx.create_module(&ast.name);
        LlvmGen {
            ast,
            ctx,
            builder,
            module,
            var_table: HashMap::new(),
            func_table: HashMap::new(),
        }
    }

    pub fn build(&mut self) -> String {
        // go through
        for val_decl in &self.ast.var_decls {
            self.gen_global_var(val_decl);
        }

        // 1st pass: gen proto
        for func_decl in &self.ast.func_decls {
            self.gen_func_proto(func_decl);
        }

        // 2nd pass: gen detail
        for func_decl in &self.ast.func_decls {
            self.gen_func(func_decl);
        }

        // to string
        self.module.print_to_string().to_string()
    }

    fn gen_global_var(&self, decl: &VarDecl) {
        todo!()
    }

    fn gen_func(&mut self, decl: &FunctionDecl) {
        let name = &decl.name.0;
        let func = self.func_table.get(name).unwrap();
        // clear table
        self.var_table.clear();

        // create basic block
        let block = self.ctx.append_basic_block(*func, "entry");
        self.builder.position_at_end(block);

        // body and return value
        self.gen_cmp_stmt(&decl.stmt.0);
        let result = self.func_table.get(name).unwrap();
        if !result.verify(true) {
            panic!("error found on func")
        }
    }

    fn gen_func_proto(&mut self, decl: &FunctionDecl) {
        // param types
        let param_types = decl
            .params
            .iter()
            .map(|param| self.gen_data_type(&param.data_type))
            .collect::<Vec<BasicTypeEnum>>();

        // function type
        let func_type = match &decl.return_type {
            FunctionType::Void => self.ctx.void_type().fn_type(&param_types, false),
            FunctionType::Data(data_type) => {
                let int_type: IntType = match data_type {
                    DataType::Int(_) => self.ctx.i32_type(),
                    DataType::Bool(_) => self.ctx.bool_type(),
                };
                int_type.fn_type(&param_types, false)
            }
        };

        let func = self.module.add_function(
            &decl.name.0,
            func_type,
            Some(inkwell::module::Linkage::External),
        );

        // set param name
        for (i, basic_value) in func.get_param_iter().enumerate() {
            let param = &decl.params[i];
            match param.data_type {
                DataType::Int(_) | DataType::Bool(_) => {
                    basic_value.into_int_value().set_name(&param.name.0)
                }
            }
        }

        // new func
        self.func_table.insert(decl.name.0.to_string(), func);
    }

    fn gen_cmp_stmt(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            match stmt {
                Stmt::FuncCallStmt(expr) => {
                    self.gen_func_call(expr);
                }

                Stmt::ReturnStmt(opt_expr) => {
                    if let Some(expr) = opt_expr {
                        let val = self.gen_expr(expr);
                        self.builder.build_return(Some(&val));
                    }

                    return;
                }

                Stmt::VarDeclStmt(decl) => {
                    if let Some(dt) = &decl.data_type {
                        let typ = self.gen_data_type(dt);
                        let name = &decl.name.0;
                        let val = self.builder.build_alloca(typ, name);
                        let _ = self.var_table.insert(name.to_string(), val);

                        // gen assign
                        self.gen_assign(name, &decl.init_value)
                    } else {
                        panic!("missing data type")
                    }
                }

                Stmt::AssignStmt(id, expr) => self.gen_assign(&id.0, expr),

                Stmt::IfElseStmt(cond, then_stmt, opt_else) => {}
            }
        }

        panic!("no return found")
    }

    fn gen_assign(&self, name: &str, expr: &Expr) {
        if let Some(ptr) = self.var_table.get(name) {
            let val = self.gen_expr(expr);
            self.builder.build_store(*ptr, val);
        } else {
            panic!("val is not found")
        }
    }

    fn gen_expr(&self, expr: &Expr) -> BasicValueEnum<'a> {
        match expr {
            Expr::LiteralExpr(literal) => self.gen_literal(literal).as_basic_value_enum(),
            Expr::VarRefExpr(name) => {
                if let Some(val) = self.var_table.get(&name.0) {
                    self.builder.build_load(*val, &name.0)
                } else {
                    panic!("val is not found: {}", name)
                }
            }
            Expr::FuncCall(expr) => self.gen_func_call(expr),
        }
    }

    fn gen_func_call(&self, expr: &FuncCallExpr) -> BasicValueEnum<'a> {
        let key = &expr.name.0;
        if let Some(func) = self.func_table.get(key) {
            let native_args: Vec<BasicValueEnum> =
                expr.args.iter().map(|ex| self.gen_expr(ex)).collect();
            let res = self.builder.build_call(*func, &native_args, key);
            return res.try_as_basic_value().left().unwrap();
        } else {
            panic!("function is not found: {}", key)
        }
    }

    fn gen_literal(&self, literal: &Literal) -> IntValue<'a> {
        match literal {
            Literal::Int(value, _) => self.ctx.i32_type().const_int(*value as u64, false),
            Literal::Bool(value, _) => self
                .ctx
                .bool_type()
                .const_int(if *value { 1 } else { 0 }, false),
        }
    }

    fn gen_data_type(&self, dt: &DataType) -> BasicTypeEnum<'a> {
        match dt {
            DataType::Int(_) => self.ctx.i32_type().as_basic_type_enum(),
            DataType::Bool(_) => self.ctx.bool_type().as_basic_type_enum(),
        }
    }
}
