use std::{fmt::Display, usize};

#[derive(Debug, Clone)]
pub struct Tok(pub TokType, pub Location);

impl Tok {
    pub fn is_typ(&self, typ: &TokType) -> bool {
        return self.0 == *typ;
    }
}

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub struct Location(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokType {
    // keyword
    KwFn,
    KwInt,
    KwBool,
    KwLet,
    KwVar,
    KwReturn,
    KwTrue,
    KwFalse,
    KwIf,
    KwElse,

    // symbols
    OpenParent,
    CloseParent,
    OpenBracket,
    CloseBracket,
    Comma,
    SemiColon,
    Colon,
    Assign,

    // id/const
    Iden(String),
    IntConst(u32),
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.0, self.1)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Display for TokType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = match self {
            TokType::KwFn => "fn",
            TokType::KwInt => "int",
            TokType::KwBool => "bool",
            TokType::KwLet => "let",
            TokType::KwVar => "var",
            TokType::KwReturn => "return",
            TokType::KwTrue => "true",
            TokType::KwFalse => "false",
            TokType::KwIf => "if",
            TokType::KwElse => "else",
            TokType::OpenParent => "(",
            TokType::CloseParent => ")",
            TokType::OpenBracket => "{",
            TokType::CloseBracket => "}",
            TokType::Comma => ",",
            TokType::SemiColon => ";",
            TokType::Colon => ":",
            TokType::Assign => "=",
            TokType::Iden(id) => id,
            TokType::IntConst(num) => return write!(f, "{}", num)
        };
        write!(f, "{}", str)
    }
}