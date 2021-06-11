use log::{debug};
use crate::{ast::{Ast, CmpStmt, DataType, Expr, FuncCallExpr, FuncParam, FunctionDecl, FunctionType, Id, Literal, Stmt, VarDecl, VarPrefix}, lexer::Lexer, token::Tok, token::TokType};
use std::iter::Peekable;

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
        let return_type = if let Some(Tok(TokType::Colon, _)) = self.lexer.peek() {
            self.consume(TokType::Colon);
            FunctionType::Data(self.parse_data_type())
        } else {
            FunctionType::Void
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
            match tok.0 {
                TokType::KwReturn => {
                    self.consume(TokType::KwReturn);
                    let expr: Option<Expr> = if self.check_peek_expr() {
                        Some(self.parse_expr())
                    } else {
                        None
                    };
                    self.consume(TokType::SemiColon);
                    return Stmt::ReturnStmt(expr);
                },
                TokType::KwLet | TokType::KwVar => {
                    let decl = self.parse_var();
                    return Stmt::VarDeclStmt(decl);
                },

                // if - else statement
                TokType::KwIf => {
                    // parse condition
                    let cond = self.parse_expr();

                    // parse else part
                    let body = self.parse_cmp_stmt();

                    // optional else part
                    let else_stmt: Option<CmpStmt> = if let Some(TokType::KwElse) = self.peek_typ() {
                        Some(self.parse_cmp_stmt())
                    } else {
                        None
                    };

                    return Stmt::IfElseStmt(cond, body, else_stmt)
                }

                // parse func call or variable assign
                TokType::Iden(_) => {
                    // assign or func call
                    let id = self.parse_id();

                    // check if open or assign
                    match self.peek_typ() {
                        Some(TokType::Assign) => {
                            // parse assign statement
                            self.consume(TokType::Assign);
                            let expr = self.parse_expr();
                            self.consume(TokType::SemiColon);
                            return Stmt::AssignStmt(id, expr);
                        },
                        Some(TokType::OpenParent) => {
                            // parse function call
                            let stmt = Stmt::FuncCallStmt(self.parse_func_call(id));
                            self.consume(TokType::SemiColon);
                            return stmt;
                        },
                        _ => panic!("expected ( or = but actual: {:?}", self.lexer.peek())
                    }
                },
                _ => panic!("unexpected token: {}", tok)
            }
        }
        
        panic!("unexpected EOF")
    }

    fn check_peek_expr(&mut self) -> bool {
        if let Some(Tok(typ, _)) = self.lexer.peek() {
            matches!(typ, TokType::Iden(_) | TokType::IntConst(_) | TokType::KwTrue | TokType::KwFalse)
        } else {
            false
        }
    }

    fn parse_expr(&mut self) -> Expr {
        if let Some(tok) = self.lexer.peek() {
            match tok.0 {
                TokType::Iden(_) => {
                    let id = self.parse_id();
                    if let Some(TokType::OpenParent) = self.peek_typ() {
                        // parse func call
                        Expr::FuncCall(self.parse_func_call(id))
                    } else {
                        debug!("parse var ref");
                        // variable/param ref
                        Expr::VarRefExpr(id)
                    }
                },
                TokType::IntConst(_) => {
                    debug!("parse int const");
                    match self.lexer.next() {
                        Some(Tok(TokType::IntConst(val), loc)) => Expr::LiteralExpr(Literal::Int(val, loc)),
                        Some(t) => panic!("expected number but {}", t),
                        _ => panic!("unexpected EOF")
                    }
                },
                TokType::KwTrue | TokType::KwFalse => {
                    debug!("parse bool const");
                    match self.lexer.next() {
                        Some(tok) => match tok {
                            Tok(TokType::KwTrue, loc) => Expr::LiteralExpr(Literal::Bool(true, loc)),
                            Tok(TokType::KwFalse, loc) => Expr::LiteralExpr(Literal::Bool(false, loc)),
                            t => panic!("expected bool constant but {:?}", t)
                        }
                        _ => panic!("unexpected EOF")
                    }
                },
                _ => panic!("expected expr but {}", tok)
            }
        } else {
            panic!("unexpected EOF")
        }
    }

    fn parse_func_call(&mut self, id: Id) -> FuncCallExpr {

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

        FuncCallExpr {
            name: id,
            args
        }
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
        let data_type: Option<DataType> = if let Some(Tok(TokType::Colon, _)) = self.lexer.peek() {
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

    fn parse_data_type(&mut self) -> DataType {
        match self.lexer.next() {
            Some(Tok(TokType::KwInt, loc)) => DataType::Int(loc),
            Some(Tok(TokType::KwBool, loc)) => DataType::Bool(loc),
            Some(t) => panic!("expected int or bool but {}", t),
            None => panic!("unexpected EOF")
        }
    }

    fn consume(&mut self, typ: TokType) {
        match self.lexer.next() {
            Some(t) if !t.is_typ(&typ) => panic!("expected {} but {}", typ, t),
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
    use crate::token::Location;

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
        assert_eq!(main.return_type, FunctionType::Data(DataType::Int(Location(1, 12))));

        // body
        let vec = &main.stmt.0;
        assert_eq!(vec.len(), 1);
        match vec.first().unwrap() {
            Stmt::ReturnStmt(Some(Expr::LiteralExpr(Literal::Int(10, Location(1, 25))))) => {},
            t => panic!("unexpected stmt: {:?}", t)
        }
    }
}