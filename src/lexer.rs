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
                    '=' => TokType::Assign,
                    t if t.is_ascii_alphabetic() => self.scan_keyword_or_id(t),
                    t if t.is_ascii_digit() => self.scan_int_const(t),
                    t => panic!("unexpected char: {} at {}", t, self.location())
                };
                Some(Tok {
                    typ,
                    line: self.line,
                    col: self.tok_col
                })
            },
            None => None
        }
    }

    fn skip_whitespace_line(&mut self) {
        loop {
            if let Some(c) = self.str.peek() {
                match c {
                    '\t' | '\n' | ' ' => {
                        self.next_char();
                    },
                    _ => break
                }
            } else {
                break
            }
        }
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
                return Some(c);
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
                t if *t == 'c' || t.is_ascii_alphanumeric() => str.push(self.next_char().unwrap()),
                _ => break
            }
        }

        // check str
        return match str.as_ref() {
            "fn" => TokType::KwFn,
            "int" => TokType::KwInt,
            "bool" => TokType::KwBool,
            "let" => TokType::KwLet,
            "var" => TokType::KwVar,
            "return" => TokType::KwReturn,
            "true" => TokType::KwTrue,
            "false" => TokType::KwFalse,
            _ => TokType::Id(str)
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
        return TokType::IntConst(str.parse().unwrap())
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