use std::iter::Peekable;
use logos::{Logos, SpannedIter};

pub type Lexer<'src> = Peekable<SpannedIter<'src, Token>>;

pub fn lex<'src>(prog: &'src str) -> Lexer<'src> {
    Token::lexer(prog).spanned().peekable()
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("fn")]
    KwFn,
    #[token("with")]
    KwWith,
    #[token("returns")]
    KwReturns,
    #[token("begin")]
    KwBegin,
    #[token("end")]
    KwEnd,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("while")]
    KwWhile,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("=")]
    Equal,
    #[token("<")]
    Less,
    #[token(">")]
    Larger,
    #[regex("'[^']'", priority = 6)]
    Char,
    #[regex("'[^']+'")]
    TooManyChars,
    #[regex("[0-9]+")]
    Num,
    #[regex("[_a-zA-Z]+")]
    Ident,
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

