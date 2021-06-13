use log::debug;

use crate::core::Location;
use crate::token::{Tok, TokType};
use std::str::Chars;
use std::iter::Peekable;

pub struct Lexer<'a> {
    str: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
    tok_col: usize
}

impl<'a> Lexer<'a> {

    fn next_token(&mut self) -> Option<Tok> {
        // skip whitespace
        self.skip_whitespace_line();

        // cache
        self.tok_col = self.col;

        // start with letter -> ID | keyword
        return match self.next_char() {
            Some(c) => {
                let typ = match c {
                    '(' => TokType::OpenParent,
                    ')' => TokType::CloseParent,
                    '{' => TokType::OpenBracket,
                    '}' => TokType::CloseBracket,
                    ',' => TokType::Comma,
                    ';' => TokType::SemiColon,
                    ':' => TokType::Colon,
                    '=' => match self.peek_char() {
                        Some('=') => {
                            self.consume_any();
                            TokType::Eq
                        },
                        _ => TokType::Assign,
                    },

                    // expression operator
                    '|' => match self.next_char() {
                        Some('|') => TokType::Or,
                        _ => panic!("expected '|'")
                    },
                    '&' => match self.next_char() {
                        Some('&') => TokType::And,
                        _ => panic!("expected '&'")
                    },
                    '!' => match self.peek_char() {
                        Some('=') => {
                            self.consume_any();
                            TokType::Neq
                        },
                        _ => TokType::Not
                    },

                    '>' => match self.peek_char() {
                        Some('=') => {
                            self.consume_any();
                            TokType::Ge
                        },
                        _ => TokType::Gt
                    },

                    '+' => TokType::Add,

                    '-' => TokType::Sub,

                    '*' => TokType::Mul,

                    '/' => TokType::Div,

                    '%' => TokType::Rem,

                    '<' => match self.peek_char() {
                        Some('=') => {
                            self.consume_any();
                            TokType::Le
                        },
                        _ => TokType::Lt
                    }

                    t if t.is_ascii_alphabetic() => self.scan_keyword_or_id(t),
                    t if t.is_ascii_digit() => self.scan_int_const(t),
                    t => panic!("unexpected char: {} at {}", t, self.location())
                };
                let tok = Tok(typ, Location(self.line, self.tok_col));
                debug!("-> {:?}", tok);
                Some(tok)
            },
            None => None
        }
    }

    fn skip_whitespace_line(&mut self) {
        while let Some(c) = self.str.peek() {
            match c {
                '\t' | '\n' | ' ' => {
                    self.next_char();
                },
                _ => break
            }
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.str.peek()
    }

    fn consume_any(&mut self) {
        self.next_char();
    }

    fn next_char(&mut self) -> Option<char> {
        match self.str.next() {
            Some(c) => {
                match c {
                    '\n' => {
                        self.line += 1;
                        self.col = 1;
                    },
                    _ => {
                        self.col += 1;
                    }
                }
                Some(c)
            }
            None => None,
        }
    }

    fn scan_keyword_or_id(&mut self, ch: char) -> TokType {
        let mut str = String::new();
        str.push(ch);

        // next is letter or digit
        while let Some(c) = self.str.peek() {
            match c {
                t if *t == '_' || t.is_ascii_alphanumeric() => str.push(self.next_char().unwrap()),
                _ => break
            }
        }

        // check str
        return match str.as_str() {
            "fn" => TokType::KwFn,
            "int" => TokType::KwInt,
            "bool" => TokType::KwBool,
            "let" => TokType::KwLet,
            "var" => TokType::KwVar,
            "return" => TokType::KwReturn,
            "true" => TokType::KwTrue,
            "false" => TokType::KwFalse,
            "if" => TokType::KwIf,
            "else" => TokType::KwElse,
            _ => TokType::Iden(str)
        }
    }

    fn scan_int_const(&mut self, ch: char) -> TokType {
        let mut str = String::new();
        str.push(ch);
        while let Some(c) = self.str.peek() {
            match c {
                t if t.is_ascii_digit() => str.push(self.next_char().unwrap()),
                _ => break
            }
        }

        // parse
        TokType::IntConst(str.parse().unwrap())
    }

    fn location(&self) -> String {
        return format!("{}:{}", self.line, self.tok_col)
    }

}

impl <'a> Lexer<'a> {
    pub fn new(str: &'a str) -> Self {
        return Lexer {
            str: str.chars().peekable(),
            line: 1,
            col: 1,
            tok_col: 1
        }
    }
}

impl <'a> Iterator for Lexer<'a> {
    type Item = Tok;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::TokType;

    #[test]
    fn test_main() {
        let lexer = Lexer::new("fn main(): int { return 1; }");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::KwFn,
            TokType::Iden("main".to_string()),
            TokType::OpenParent,
            TokType::CloseParent,
            TokType::Colon,
            TokType::KwInt,
            TokType::OpenBracket,
            TokType::KwReturn,
            TokType::IntConst(1),
            TokType::SemiColon,
            TokType::CloseBracket,
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_params() {
        let lexer = Lexer::new("fn foo(x_1: int, y2: bool) {}");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::KwFn,
            TokType::Iden("foo".to_string()),
            TokType::OpenParent,
            TokType::Iden("x_1".to_string()),
            TokType::Colon,
            TokType::KwInt,
            TokType::Comma,
            TokType::Iden("y2".to_string()),
            TokType::Colon,
            TokType::KwBool,
            TokType::CloseParent,
            TokType::OpenBracket,
            TokType::CloseBracket,
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_stmts() {
        let lexer = Lexer::new("fn bar_2() { let x: int = 100; let y: bool = false; x = 987; }");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::KwFn,
            TokType::Iden("bar_2".to_string()),
            TokType::OpenParent,
            TokType::CloseParent,
            TokType::OpenBracket,

            TokType::KwLet,
            TokType::Iden("x".to_string()),
            TokType::Colon,
            TokType::KwInt,
            TokType::Assign,
            TokType::IntConst(100),
            TokType::SemiColon,

            TokType::KwLet,
            TokType::Iden("y".to_string()),
            TokType::Colon,
            TokType::KwBool,
            TokType::Assign,
            TokType::KwFalse,
            TokType::SemiColon,

            TokType::Iden("x".to_string()),
            TokType::Assign,
            TokType::IntConst(987),
            TokType::SemiColon,
            TokType::CloseBracket,
        ];

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_if_else() {
        let lexer = Lexer::new("fn t() { if true {} else {} }");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::KwFn,
            TokType::Iden("t".to_string()),
            TokType::OpenParent,
            TokType::CloseParent,
            TokType::OpenBracket,
            TokType::KwIf,
            TokType::KwTrue,
            TokType::OpenBracket,
            TokType::CloseBracket,
            TokType::KwElse,
            TokType::OpenBracket,
            TokType::CloseBracket,
            TokType::CloseBracket
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_digit_letter() {
        let lexer = Lexer::new("12main");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::IntConst(12),
            TokType::Iden("main".to_string()),
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_letter_digit() {
        let lexer = Lexer::new("main12");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::Iden("main12".to_string()),
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_op_addsub() {
        let lexer = Lexer::new("3 + 4 - 5");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::IntConst(3),
            TokType::Add,
            TokType::IntConst(4),
            TokType::Sub,
            TokType::IntConst(5),
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_op_muldiv() {
        let lexer = Lexer::new("3 * 4 / 5 % 6");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::IntConst(3),
            TokType::Mul,
            TokType::IntConst(4),
            TokType::Div,
            TokType::IntConst(5),
            TokType::Rem,
            TokType::IntConst(6),
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_op_logic() {
        let lexer = Lexer::new("a && b || c && !d");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::Iden("a".to_string()),
            TokType::And,
            TokType::Iden("b".to_string()),
            TokType::Or,
            TokType::Iden("c".to_string()),
            TokType::And,
            TokType::Not,
            TokType::Iden("d".to_string()),
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_op_eq() {
        let lexer = Lexer::new("> >= <= < == !=");
        let actual: Vec<TokType> = lexer.map(|t| t.0).collect();
        let expected = vec![
            TokType::Gt,
            TokType::Ge,
            TokType::Le,
            TokType::Lt,
            TokType::Eq,
            TokType::Neq,
        ];
        assert_eq!(actual, expected)
    }
}