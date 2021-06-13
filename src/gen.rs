use std::collections::HashMap;

use inkwell::{builder::Builder, context::Context, module::Module, types::{BasicType, BasicTypeEnum}, values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue}, IntPredicate};

use crate::ast::{Ast, DataType, Expr, FunctionDecl, Literal, Stmt, VarDecl, Id, DataTypeLoc, BinOp, UnaryOp};

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

    pub fn build(&mut self) -> &Module<'a> {
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

        // return
        &self.module
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
            DataTypeLoc(DataType::Void, _) => self.ctx.void_type().fn_type(&param_types, false),
            t => self.gen_data_type(t).fn_type(&param_types, false)
        };

        let func = self.module.add_function(
            &decl.name.0,
            func_type,
            Some(inkwell::module::Linkage::External),
        );

        // set param name
        for (i, basic_value) in func.get_param_iter().enumerate() {
            let param = &decl.params[i];
            basic_value.into_int_value().set_name(&param.name.0)
        }

        // new func
        self.func_table.insert(decl.name.0.to_string(), func);
    }

    fn gen_cmp_stmt(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            match stmt {
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
                        let id = &decl.name;
                        let val = self.builder.build_alloca(typ, &id.0);
                        let _ = self.var_table.insert(id.0.to_string(), val);

                        // gen assign
                        self.gen_assign(id, &decl.init_value);
                    } else {
                        panic!("missing data type")
                    }
                }

                Stmt::IfElseStmt(cond, then_stmt, opt_else) => {}

                Stmt::Expr(expr) => {
                    self.gen_expr(expr);
                }
            }
        }

        panic!("no return found")
    }

    fn gen_expr(&self, expr: &Expr) -> BasicValueEnum<'a> {
        match expr {
            Expr::Literal(literal) => self.gen_literal(literal).as_basic_value_enum(),
            Expr::VarRef(name) => {
                if let Some(val) = self.var_table.get(&name.0) {
                    self.builder.build_load(*val, &name.0)
                } else {
                    panic!("val is not found: {}", name)
                }
            }
            Expr::FuncCall(id, args) => self.gen_func_call(id, args),
            Expr::Assign(id, expr) => self.gen_assign(id, expr),
            Expr::BinOpExpr(lhs, op, rhs) => self.gen_op_expr(lhs, op, rhs),
            Expr::Unary(op, body) => match op {
                UnaryOp::Not => panic!("not op is not yet supported"),
                UnaryOp::Sub => {
                    let rval = self.gen_expr(body).into_int_value();
                    self.builder.build_int_sub(self.ctx.i32_type().const_zero(), rval, "tmpunary").as_basic_value_enum()
                }
            }
        }
    }

    fn gen_op_expr(&self, lhs: &Expr, op: &BinOp, rhs: &Expr) -> BasicValueEnum<'a> {

        let lval = self.gen_expr(lhs).into_int_value();
        let rval = self.gen_expr(rhs).into_int_value();

        // int
        match op {
            // arith
            BinOp::Add => self.builder.build_int_add(lval, rval, "tmpadd").as_basic_value_enum(),
            BinOp::Sub => self.builder.build_int_sub(lval, rval, "tmpsub").as_basic_value_enum(),
            BinOp::Mul => self.builder.build_int_mul(lval, rval, "tmpmul").as_basic_value_enum(),
            BinOp::Div => self.builder.build_int_signed_div(lval, rval, "tmpdiv").as_basic_value_enum(),
            BinOp::Rem => self.builder.build_int_signed_rem(lval, rval, "tmprem").as_basic_value_enum(),

            BinOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, lval, rval, "tmpgt").as_basic_value_enum(),
            BinOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lval, rval, "tmplt").as_basic_value_enum(),
            BinOp::Ge => self.builder.build_int_compare(IntPredicate::SGE, lval, rval, "tmpge").as_basic_value_enum(),
            BinOp::Le => self.builder.build_int_compare(IntPredicate::SLE, lval, rval, "tmple").as_basic_value_enum(),

            BinOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, lval, rval, "tmpeq").as_basic_value_enum(),
            BinOp::Neq => self.builder.build_int_compare(IntPredicate::NE, lval, rval, "tmpne").as_basic_value_enum(),

            BinOp::Or | BinOp::And => panic!("And and Or must be converted to basic blocs, not handled by code-gen")
        }
    }

    fn gen_func_call(&self, id: &Id, args: &[Expr]) -> BasicValueEnum<'a> {
        let key = &id.0;
        if let Some(func) = self.func_table.get(key) {
            let native_args: Vec<BasicValueEnum> =
                args.iter().map(|ex| self.gen_expr(ex)).collect();
            let res = self.builder.build_call(*func, &native_args, key);
            return res.try_as_basic_value().left().unwrap();
        } else {
            panic!("function is not found: {}", key)
        }
    }

    fn gen_assign(&self, name: &Id, expr: &Expr) -> BasicValueEnum<'a> {
        if let Some(ptr) = self.var_table.get(&name.0) {
            let val = self.gen_expr(expr);
            self.builder.build_store(*ptr, val);
            val.as_basic_value_enum()
        } else {
            panic!("val is not found")
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

    fn gen_data_type(&self, dt: &DataTypeLoc) -> BasicTypeEnum<'a> {
        match &dt.0 {
            DataType::Int => self.ctx.i32_type().as_basic_type_enum(),
            DataType::Bool => self.ctx.bool_type().as_basic_type_enum(),
            t => panic!("type is not supported: {:?}", t),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::gen::LlvmGen;
    use inkwell::context::Context;

    fn gen(str: &str) -> String {
        let lexer = Lexer::new(str);
        let ast = Parser::new(lexer).parse("test".to_string());
        let ctx = Context::create();
        let mut gen = LlvmGen::new(&ast, &ctx);
        gen.build().print_to_string().to_string()
    }

    #[test]
    fn test_main() {
        assert_eq!(gen("fn main(): int { return 10; }").is_empty(), false)
    }

    #[test]
    fn parse_valid_1() {
        assert_eq!(gen("fn main(): bool { let x: bool = false; return x = true; }").is_empty(), false)
    }

    #[test]
    fn parse_valid_2() {
        assert_eq!(gen("fn foo(): int { let x: int = 0; let y: int = x = 2; x = y;  return 0;}").is_empty(), false)
    }
}