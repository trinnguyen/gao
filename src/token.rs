use std::{fmt::Display, usize};
use serde::{Serialize, Deserialize, Serializer};
use crate::core::Location;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tok(pub TokType, pub Location);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

    // expression operator
    Or,
    And,
    Eq,
    Neq,
    Gt,
    Lt,
    Ge,
    Le,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // unary
    Not,

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
            TokType::IntConst(num) => return write!(f, "{}", num),
            TokType::Or => "||",
            TokType::And => "&&",
            TokType::Eq => "==",
            TokType::Neq => "!=",
            TokType::Gt => ">",
            TokType::Lt => "<",
            TokType::Ge => ">=",
            TokType::Le => "<=",
            TokType::Add => "+",
            TokType::Sub => "-",
            TokType::Mul => "*",
            TokType::Div => "/",
            TokType::Mod => "%",
            TokType::Not => "!",
        };
        write!(f, "{}", str)
    }
}