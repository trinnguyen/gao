use inkwell::module::Module;
use inkwell::OptimizationLevel;

pub fn execute_jit(module: &Module) -> i32 {
    let ee = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

    // find main func in module
    let name = "main";
    let opt_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i32>(name) };
    match opt_fn {
        Ok(func) => unsafe { func.call() }
        Err(err) => panic!("failed to lookup main function: {}", err)
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;
    use crate::gen::LlvmGen;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::jit::execute_jit;

    fn eval(str: &str) -> i32 {
        let mut parser = Parser::new(Lexer::new(str));
        let ast = parser.parse("test.gao".to_string());
        let context = Context::create();
        let mut gen = LlvmGen::new(&ast, &context);
        execute_jit(gen.build())
    }

    #[test]
    fn test_expr_add_0() {
        assert_eq!(eval("fn main(): int { return 1 + 2; }"), 3);
    }

    #[test]
    fn test_expr_add_1() {
        assert_eq!(eval("fn main(): int { return - 1 + 2; }"), 1);
    }

    #[test]
    fn test_expr_add_2() {
        assert_eq!(eval("fn main(): int { return - (1 + 2 + 3); }"), -6);
    }

    #[test]
    fn test_expr3() {
        assert_eq!(eval("fn main(): int { return -1 + 2 - 4 * 5 / 6; }"), -2);
    }

    #[test]
    fn test_expr4() {
        assert_eq!(eval("fn main(): int { return 1 + 2 + 3 * 4; }"), 15);
    }

    #[test]
    fn test_expr5() {
        assert_eq!(eval("fn main(): int { return (2 + 3) * 4; }"), 20);
    }

    #[test]
    fn test_expr6() {
        assert_eq!(eval("fn main(): int { let x: int = 3; let y: int = x + 3; return x - y; } "), -3);
    }

    #[test]
    fn test_call() {
        assert_eq!(eval("fn main(): int { return foo(); } fn foo(): int { let x: int = 100; return x * 100 ; }  "), 10000);
    }
}