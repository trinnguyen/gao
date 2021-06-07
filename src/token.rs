#[derive(Debug, Clone)]
pub struct Tok {
    pub typ: TokType,
    pub line: usize,
    pub col: usize
}

#[derive(Debug, Clone)]
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
    Id(String),
    IntConst(u32),
}