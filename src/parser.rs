use log::{debug};
use crate::{ast::{Ast, CmpStmt, DataType, Expr, FuncParam, FunctionDecl, Id, Literal, Stmt, VarDecl, VarPrefix}, lexer::Lexer, token::Tok, token::TokType};
use std::iter::Peekable;
use crate::ast::{DataTypeLoc, UnaryOp, BinOp};
use crate::core::Location;
use crate::ast::Expr::Unary;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl <'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable()
        }
    }

    pub fn parse(&mut self, name: String) -> Ast {
        let mut var_decls: Vec<VarDecl> = Vec::new();
        let mut func_decls: Vec<FunctionDecl> = Vec::new();

        // var decl or function
        while let Some(tok) = self.lexer.peek() {
            match tok.0 {
                TokType::KwFn => {
                    func_decls.push(self.parse_fn());
                },
                TokType::KwLet | TokType::KwVar => {
                    var_decls.push(self.parse_var());
                },
                _ => panic!("unexpected token {}", tok)
            }
        }

        // return
        Ast {
            name,
            var_decls,
            func_decls
        }
    }

    fn parse_fn(&mut self) -> FunctionDecl {
        debug!("parse fn");
        self.consume(TokType::KwFn);
        let name = self.parse_id();

        // open
        self.consume(TokType::OpenParent);

        // parse params
        let mut params: Vec<FuncParam> = Vec::new();
        if self.check_peek_id() {
            params.push(self.parse_param());

            // next
            while let Some(Tok(TokType::Comma, _)) = self.lexer.peek() {
                self.consume(TokType::Comma);
                params.push(self.parse_param());
            }
        }

        // close
        self.consume(TokType::CloseParent);

        // return type (optional)
        let return_type: DataTypeLoc = if let Some(Tok(TokType::Colon, _)) = self.lexer.peek() {
            self.consume(TokType::Colon);
            self.parse_data_type()
        } else {
            DataTypeLoc(DataType::Void, Location(0, 0))
        };

        // cmp stmt
        let stmt = self.parse_cmp_stmt();

        FunctionDecl {
            name,
            params,
            return_type,
            stmt
        }
    }

    fn parse_cmp_stmt(&mut self) -> CmpStmt {
        let loc = self.lexer.peek().unwrap().1;
        self.consume(TokType::OpenBracket);
        
        let mut stmts: Vec<Stmt> = Vec::new();
        while self.check_peek_stmt() {
            stmts.push(self.parse_stmt());
        }

        self.consume(TokType::CloseBracket);
        CmpStmt(stmts, loc)
    }

    fn check_peek_stmt(&mut self) -> bool {
        
        if let Some(typ) = self.peek_typ() {
            return match typ {
                TokType::KwReturn | TokType::KwLet | TokType::KwVar | TokType::KwIf => true,
                _ => self.check_peek_expr()
            }
        }

        false
    }

    fn parse_stmt(&mut self) -> Stmt {

        // return stmt
        if let Some(tok) =  self.lexer.peek() {
            return match tok.0 {
                TokType::KwReturn => {
                    self.consume(TokType::KwReturn);
                    let expr: Option<Expr> = if self.check_peek_expr() {
                        Some(self.parse_expr())
                    } else {
                        None
                    };
                    self.consume(TokType::SemiColon);
                    Stmt::ReturnStmt(expr)
                },
                TokType::KwLet | TokType::KwVar => {
                    let decl = self.parse_var();
                    Stmt::VarDeclStmt(decl)
                },

                // if - else statement
                TokType::KwIf => {
                    // parse condition
                    self.consume_any();
                    let cond = self.parse_expr();

                    // parse else part
                    let body = self.parse_cmp_stmt();

                    // optional else part
                    let else_stmt: Option<CmpStmt> = if let Some(TokType::KwElse) = self.peek_typ() {
                        self.consume_any();
                        Some(self.parse_cmp_stmt())
                    } else {
                        None
                    };

                    Stmt::IfElseStmt(cond, body, else_stmt)
                },

                // expr
                _ => {
                    let expr = self.parse_expr();
                    self.consume(TokType::SemiColon);
                    Stmt::Expr(expr)
                }
            }
        }
        
        panic!("unexpected EOF")
    }

    fn check_peek_expr(&mut self) -> bool {
        if let Some(Tok(typ, _)) = self.lexer.peek() {
            matches!(typ,
                TokType::Iden(_) // fun call, var ref
                | TokType::IntConst(_) // literal
                | TokType::KwTrue // literal
                | TokType::KwFalse // literal
                | TokType::Not // unary
                | TokType::Sub // unary
                | TokType::OpenParent // unary
            )
        } else {
            false
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Expr {
        let mut lhs = self.parse_and_expr();
        while let Some(TokType::Or) = self.peek_typ() {
            self.consume_any();
            let rhs = self.parse_and_expr();
            lhs = Expr::BinOpExpr(Box::new(lhs), BinOp::Or, Box::new(rhs))
        }

        lhs
    }

    fn parse_and_expr(&mut self) -> Expr {
        let mut lhs = self.parse_equality_expr();
        while let Some(TokType::And) = self.peek_typ() {
            self.consume_any();
            let rhs = self.parse_equality_expr();
            lhs = Expr::BinOpExpr(Box::new(lhs), BinOp::And, Box::new(rhs))
        }

        lhs
    }

    fn parse_equality_expr(&mut self) -> Expr {
        let mut lhs = self.parse_relational_expr();
        while let Some(tok) = self.peek_typ() {
            let op = match tok {
                TokType::Eq => BinOp::Eq,
                TokType::Neq => BinOp::Neq,
                _ => break
            };
            self.consume_any();
            let rhs = self.parse_relational_expr();
            lhs = Expr::BinOpExpr(Box::new(lhs), op, Box::new(rhs))
        }

        lhs
    }

    fn parse_relational_expr(&mut self) -> Expr {
        let mut lhs = self.parse_addsub_expr();
        while let Some(tok) = self.peek_typ() {
            let op = match tok {
                TokType::Gt => BinOp::Gt,
                TokType::Ge => BinOp::Ge,
                TokType::Lt => BinOp::Lt,
                TokType::Le => BinOp::Le,
                _ => break
            };
            self.consume_any();
            let rhs = self.parse_addsub_expr();
            lhs = Expr::BinOpExpr(Box::new(lhs), op, Box::new(rhs))
        }

        lhs
    }

    fn parse_addsub_expr(&mut self) -> Expr {
        let mut lhs = self.parse_muldiv_expr();
        while let Some(tok) = self.peek_typ() {
            let op = match tok {
                TokType::Add => BinOp::Add,
                TokType::Sub => BinOp::Sub,
                _ => break
            };
            self.consume_any();
            let rhs = self.parse_muldiv_expr();
            lhs = Expr::BinOpExpr(Box::new(lhs), op, Box::new(rhs))
        }

        lhs
    }

    fn parse_muldiv_expr(&mut self) -> Expr {
        let mut lhs = self.parse_primary();
        while let Some(tok) = self.peek_typ() {
            let op = match tok {
                TokType::Mul => BinOp::Mul,
                TokType::Div => BinOp::Div,
                TokType::Rem => BinOp::Rem,
                _ => break
            };
            self.consume_any();
            let rhs = self.parse_primary();
            lhs = Expr::BinOpExpr(Box::new(lhs), op, Box::new(rhs))
        }

        lhs
    }

    fn parse_primary(&mut self) -> Expr {
        if let Some(tok) = self.lexer.peek() {
            match tok.0 {

                TokType::Not | TokType::Sub => {
                    // parse unary
                    let typ = self.lexer.next().unwrap().0;
                    let op = match typ {
                        TokType::Not => UnaryOp::Not,
                        _ => UnaryOp::Sub
                    };
                    let expr = self.parse_primary();
                    Expr::Unary(op, Box::new(expr))
                }

                TokType::Iden(_) => {
                    let id = self.parse_id();
                    match self.peek_typ() {
                        // parse func call
                        Some(TokType::OpenParent) => Expr::FuncCall(id, self.parse_args()),
                        Some(TokType::Assign) => {
                            self.consume_any();
                            Expr::Assign(id, Box::new(self.parse_expr()))
                        },
                        _ => Expr::VarRef(id)
                    }
                }

                TokType::IntConst(_) => {
                    debug!("parse int const");
                    match self.lexer.next() {
                        Some(Tok(TokType::IntConst(val), loc)) => Expr::Literal(Literal::Int(val, loc)),
                        Some(t) => panic!("expected number but {}", t),
                        _ => panic!("unexpected EOF")
                    }
                }

                TokType::KwTrue | TokType::KwFalse => {
                    debug!("parse bool const");
                    match self.lexer.next() {
                        Some(tok) => match tok {
                            Tok(TokType::KwTrue, loc) => Expr::Literal(Literal::Bool(true, loc)),
                            Tok(TokType::KwFalse, loc) => Expr::Literal(Literal::Bool(false, loc)),
                            t => panic!("expected bool constant but {:?}", t)
                        }
                        _ => panic!("unexpected EOF")
                    }
                }

                TokType::OpenParent => {
                    self.consume_any();
                    let expr = self.parse_expr();
                    self.consume(TokType::CloseParent);
                    expr
                }

                _ => panic!("expected expr but {}", tok)
            }
        } else {
            panic!("unexpected EOF")
        }
    }

    fn parse_args(&mut self) -> Vec<Expr> {

        self.consume(TokType::OpenParent);

        // argument
        let mut args = Vec::<Expr>::new();
        if self.check_peek_expr() {
            args.push(self.parse_expr());
            while let Some(Tok(TokType::Comma, _)) = self.lexer.peek() {
                self.consume_any();
                args.push(self.parse_expr());
            }
        }

        self.consume(TokType::CloseParent);

        args
    }

    fn parse_var(&mut self) -> VarDecl {
        debug!("parse var/let");
        // parse prefix
        let prefix: VarPrefix = match self.lexer.next() {
            Some(Tok(TokType::KwVar, loc)) => VarPrefix::Var(loc),
            Some(Tok(TokType::KwLet, loc)) => VarPrefix::Let(loc),
            Some(t) => panic!("expected var or let but {}", t),
            _ => panic!("unexpected EOF")
        };

        // name
        let name = self.parse_id();

        // option data type
        let data_type: Option<DataTypeLoc> = if let Some(Tok(TokType::Colon, _)) = self.lexer.peek() {
            self.consume(TokType::Colon);
            Some(self.parse_data_type())
        } else {
            None
        };

        // init value
        self.consume(TokType::Assign);
        let init_value = self.parse_expr();

        // finish
        self.consume(TokType::SemiColon);

        VarDecl {
            prefix,
            name,
            data_type,
            init_value
        }
    }

    fn check_peek_id(&mut self) -> bool {
        return matches!(self.lexer.peek(), Some(Tok(TokType::Iden(_), _)))
    }

    fn parse_param(&mut self) -> FuncParam {
        let name = self.parse_id();
        self.consume(TokType::Colon);
        let data_type = self.parse_data_type();
        FuncParam {
            name,
            data_type
        }
    }

    fn parse_id(&mut self) -> Id {
        match self.lexer.next().unwrap() {
            Tok(TokType::Iden(id), loc) => {
                Id(id, loc)
            },
            t => panic!("expected Id but {}", t)
        }
    }

    fn parse_data_type(&mut self) -> DataTypeLoc {
        match self.lexer.next() {
            Some(Tok(TokType::KwInt, loc)) => DataTypeLoc(DataType::Int, loc),
            Some(Tok(TokType::KwBool, loc)) => DataTypeLoc(DataType::Bool, loc),
            Some(t) => panic!("expected int or bool but {}", t),
            None => panic!("unexpected EOF")
        }
    }

    fn consume(&mut self, typ: TokType) {
        match self.lexer.next() {
            Some(t) if t.0 != typ => panic!("expected '{}' but '{}'", typ, t),
            Some(_) => {},
            None => panic!("unexpected EOF")
        }
    }

    fn consume_any(&mut self) {
        let _ = self.lexer.next();
    }

    fn peek_typ(&mut self) -> Option<&TokType> {
        return self.lexer.peek().map(|tok| &tok.0);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::lexer::Lexer;
    use crate::ast::*;
    use crate::core::Location;

    fn parse(str: &str) -> Ast {
        let lexer = Lexer::new(str);
        Parser::new(lexer).parse("test".to_string())
    }

    #[test]
    fn test_name() {
        let ast = parse("fn main(): int { return 1; }");
        assert_eq!(ast.name, "test")
    }

    #[test]
    fn test_main() {
        let ast = parse("fn main(): int { return 10; }");
        assert_eq!(ast.func_decls.len(), 1);
        assert_eq!(ast.var_decls.len(), 0);

        // fun
        let main = ast.func_decls.first().unwrap();
        assert_eq!(main.name, Id("main".to_string(), Location(1, 4)));
        assert_eq!(main.params.len(), 0);
        assert_eq!(main.return_type, DataTypeLoc(DataType::Int, Location(1, 12)));

        // body
        let vec = &main.stmt.0;
        assert_eq!(vec.len(), 1);
        match vec.first().unwrap() {
            Stmt::ReturnStmt(Some(Expr::Literal(Literal::Int(10, Location(1, 25))))) => {},
            t => panic!("unexpected stmt: {:?}", t)
        }
    }

    #[test]
    fn parse_valid_1() {
        let ast = parse("fn main() { let x = false; x = true; } ");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 2);
    }

    #[test]
    fn parse_valid_2() {
        let ast = parse("fn foo() { let x = 0; let y = x = 2; let x = y; } ");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 3);
    }

    #[test]
    fn parse_valid_3() {
        let ast = parse("fn foo() { put(1); let x = put(2); let b = bar(); } ");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 3);
    }

    #[test]
    fn parse_if_else() {
        let ast = parse("fn foo() { if true { } else {} }");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 1);
    }

    #[test]
    fn parse_if_else_1() {
        let ast = parse("fn foo() { if true { if false {} else {} } } ");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 1);
    }

    #[test]
    fn parse_if_else_2() {
        let ast = parse("fn foo(a: bool): int { if a { let x: int = 1; } else { return 1; } return 0;  } ");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 2);
    }

    #[test]
    fn parse_or() {
        let ast = parse("fn foo() { let x: bool = true || false; }");
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 1);
    }

    #[test]
    fn parse_expr() {
        let src = "fn main() {
    let x = -1 + 2 - 4 * 5 / 6 % 7;
    let x1 = -(1+2);
    let y = !a && b || c;
    let c = 1 >= 2 == 1 > 2;
    let d = 1 < 2 != 1 <= 2;
}";
        let ast = parse(src);
        assert_eq!(ast.func_decls.first().unwrap().stmt.0.len(), 5);
    }

    #[test]
    fn parse_expr_2() {
        let _ = parse("fn main(): int { return -1; }");
    }

    #[test]
    fn parse_expr_3() {
        let _ = parse("fn main(): bool { return !false; }");
    }

    #[test]
    fn parse_expr_4() {
        let _ = parse("fn main(): int { return (2 + 3) * 4; } ");
    }
}