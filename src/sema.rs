use crate::interpret::Value;
use crate::token::{Lexer, Token};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Equal,
    Less,
    Larger,
}

#[derive(Debug, Clone)]
pub enum Atom {
    Num(u64),
    Bool(bool),
    Fn(String),
    Op(Op),
    If(Vec<Atom>, Option<Vec<Atom>>),
    While(Vec<Atom>),
}

pub enum Function {
    Code(Vec<Atom>, Option<Vec<String>>, Option<String>),
    Builtin(Box<dyn Fn(&mut Vec<Value>)>),
}

pub struct Sema {
    functions: HashMap<String, Function>,
}

impl Sema {
    pub fn insert(&mut self, functions: HashMap<String, Function>) {
        for (n, f) in functions {
            self.functions.entry(n).or_insert(f);
        }
    }

    pub fn from_ast(ast: Ast) -> Self {
        fn extract_funcs(ast: Ast, functions: &mut HashMap<String, Function>) {
            match ast {
                Ast::Prog(funcs) => funcs
                    .into_iter()
                    .for_each(|ast| extract_funcs(ast, functions)),
                Ast::Fn(func, atoms, params, ret) => {
                    functions.insert(func, Function::Code(atoms, params, ret));
                }
            }
        }

        let mut functions = HashMap::new();
        extract_funcs(ast, &mut functions);

        Sema { functions }
    }

    pub fn functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Prog(Vec<Ast>),
    Fn(String, Vec<Atom>, Option<Vec<String>>, Option<String>),
}

impl Ast {
    fn parse_block<'src>(prog: &'src str, lex: &mut Lexer<'src>, in_if: bool) -> Vec<Atom> {
        let mut atoms = vec![];
        while let Some((token, span)) = lex.next() {
            atoms.push(match token {
                Token::Plus => Atom::Op(Op::Plus),
                Token::Minus => Atom::Op(Op::Minus),
                Token::Equal => Atom::Op(Op::Equal),
                Token::Less => Atom::Op(Op::Less),
                Token::Larger => Atom::Op(Op::Larger),
                Token::KwTrue => Atom::Bool(true),
                Token::KwFalse => Atom::Bool(false),
                Token::Num => Atom::Num(prog[span].parse().unwrap()),
                Token::Ident => Atom::Fn(prog[span].to_string()),
                Token::KwEnd => return atoms,
                Token::KwWhile => Atom::While(Ast::parse_block(prog, lex, false)),
                Token::KwIf => {
                    let then = Ast::parse_block(prog, lex, true);
                    let otherwise = match lex.peek() {
                        Some((Token::KwElse, _)) => {
                            lex.next();
                            Some(Ast::parse_block(prog, lex, false))
                        }
                        _ => None,
                    };
                    Atom::If(then, otherwise)
                }
                Token::KwElse => {
                    if in_if {
                        return atoms;
                    }
                    panic!("found `else` outside of `if`");
                }
                Token::Char => {
                    Atom::Num(prog[span.start + 1..span.end - 1].parse::<char>().unwrap() as u64)
                }
                token => unimplemented!("{:?} ({:?})", token, &prog[span]),
            });

            if let Some((Token::KwElse, _)) = lex.peek() {
                return atoms;
            }
        }
        panic!("Unexpected EOF");
    }

    fn parse_fn<'src>(prog: &'src str, lex: &mut Lexer<'src>) -> Ast {
        let ident = match lex.next() {
            Some((Token::Ident, span)) => prog[span].to_string(),
            t => unimplemented!("{:?}", t),
        };

        let params = match lex.peek() {
            Some((Token::KwWith, _)) => {
                lex.next();
                let mut params = vec![];
                while let Some((Token::Ident, span)) = lex.peek() {
                    params.push(prog[span.clone()].to_string());
                    lex.next();
                }
                Some(params)
            }
            _ => None,
        };

        let ret = match lex.peek() {
            Some((Token::KwReturns, _)) => {
                lex.next();
                Some(prog[lex.next().unwrap().1].to_string())
            }
            _ => None,
        };

        match lex.next() {
            Some((Token::KwBegin, _)) => (),
            t => unimplemented!("{:?}", t),
        }

        Ast::Fn(ident, Ast::parse_block(prog, lex, false), params, ret)
    }

    pub fn parse<'src>(prog: &'src str, lex: &mut Lexer<'src>) -> Ast {
        let mut items = vec![];
        while let Some((token, _)) = lex.next() {
            match token {
                Token::KwFn => items.push(Ast::parse_fn(prog, lex)),
                _ => unimplemented!("Expected function, found: {token:?}"),
            };
        }
        Ast::Prog(items)
    }
}
