use crate::interpret::Value;
use crate::token::{Lexer, Token};
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Equal,
    Less,
    Greater,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Num(u64),
    Bool(bool),
    Fn(String),
    Op(Op),
    If(Vec<Atom>, Option<Vec<Atom>>),
    While(Vec<Atom>),
}

pub enum Function {
    Code(Vec<Atom>, Vec<String>, Vec<String>),
    Builtin(Box<dyn Fn(&mut Vec<Value>)>),
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Code(block, params, rets) => {
                write!(f, "Code({block:?}, {params:?}, {rets:?})")
            }
            Function::Builtin(_) => write!(f, "<builtin>"),
        }
    }
}

pub struct Sema {
    functions: HashMap<String, Function>,
}

impl Sema {
    pub fn type_check(&self, func: &str) -> Result<(), String> {
        let (block, params, ret) = match self.functions.get(func) {
            Some(Function::Code(block, params, ret)) => (block, params, ret),
            Some(Function::Builtin(..)) => return Ok(()),
            None => return Err("There is no such funtion".into()),
        };
        let type_stack = params.clone();
        let ret_stack = self.get_return_stack_of_block(block, type_stack)?;
        if &ret_stack == ret {
            Ok(())
        } else {
            Err(format!("Too many values left over on stack: `{ret_stack:?}`"))
        }
    }

    fn get_return_stack_of_block(
        &self,
        block: &Vec<Atom>,
        mut type_stack: Vec<String>,
    ) -> Result<Vec<String>, String> {
        for atom in block {
            match atom {
                Atom::Num(_) => type_stack.push("int".into()),
                Atom::Bool(_) => type_stack.push("bool".into()),
                Atom::Fn(f) => match self.functions.get(f) {
                    Some(Function::Code(_, params, ret)) => {
                        for param in params.into_iter().rev() {
                            let arg = match type_stack.pop() {
                                Some(t) => t,
                                None => {
                                    return Err(format!("Too few values on stack to call `{f}`"))
                                }
                            };
                            if !param.is_empty() && &arg != param {
                                return Err(format!("Mismatched param when calling `{f}`"));
                            }
                        }

                        type_stack.extend_from_slice(ret);
                    }
                    _ => match f.as_str() {
                        "printc" => {
                            if type_stack.pop().is_none() {
                                return Err(format!("Too few values for `{f}`"));
                            }
                        }
                        "print" => {
                            if type_stack.pop().is_none() {
                                return Err(format!("Too few values for `{f}`"));
                            }
                        }
                        "dup" => {
                            if let Some(top) = type_stack.pop() {
                                type_stack.push(top.clone());
                                type_stack.push(top);
                            } else {
                                return Err(format!("Too few values for `{f}`"));
                            }
                        }
                        "over" => {
                            let fst = match type_stack.pop() {
                                Some(t) => t,
                                None => return Err(format!("Too few values for `{f}`")),
                            };
                            let snd = match type_stack.pop() {
                                Some(t) => t,
                                None => return Err(format!("Too few values for `{f}`")),
                            };
                            type_stack.push(snd.clone());
                            type_stack.push(fst);
                            type_stack.push(snd);
                        }
                        "drop" => {
                            if type_stack.pop().is_none() {
                                return Err(format!("Too few values for `{f}`"));
                            }
                        }
                        "rot" => {
                            let fst = match type_stack.pop() {
                                Some(t) => t,
                                None => return Err(format!("Too few values for `{f}`")),
                            };
                            let snd = match type_stack.pop() {
                                Some(t) => t,
                                None => return Err(format!("Too few values for `{f}`")),
                            };
                            type_stack.push(fst);
                            type_stack.push(snd);
                        }
                        _ => {
                            return Err(format!(
                                "builtin `{f}` has not been implemented for type_checking"
                            ))
                        }
                    },
                },
                Atom::Op(op @ (Op::Plus | Op::Minus | Op::Equal | Op::Less | Op::Greater)) => {
                    if Some("int") != type_stack.pop().as_deref() {
                        return Err(format!("Too few/wrong arguments to `{op:?}`"));
                    }
                    if Some("int") != type_stack.pop().as_deref() {
                        return Err(format!("Too few/wrong arguments to `{op:?}`"));
                    }
                    match op {
                        Op::Plus | Op::Minus => type_stack.push("int".to_string()),
                        Op::Equal | Op::Less | Op::Greater => type_stack.push("bool".to_string()),
                    }
                }
                Atom::If(then, otherwise) => {
                    if Some("bool") != type_stack.pop().as_deref() {
                        return Err("if statement needs a conditional".into());
                    }
                    let then_ret = self.get_return_stack_of_block(then, type_stack.clone())?;
                    match otherwise {
                        Some(otherwise) => {
                            let otherwise_ret =
                                self.get_return_stack_of_block(otherwise, type_stack)?;

                            if then_ret != otherwise_ret {
                                return Err(
                                    "Then and else branches must end with the same stack".into()
                                );
                            }
                            type_stack = otherwise_ret;
                        }
                        None => {
                            if then_ret != type_stack {
                                return Err("Lone if statement must not change the stack".into());
                            }
                        }
                    }
                }
                Atom::While(block) => {
                    if Some("bool") != type_stack.pop().as_deref() {
                        return Err("while statement must start with a conditional on the top of the stack".into());
                    }
                    let mut ret = self.get_return_stack_of_block(block, type_stack.clone())?;
                    if Some("bool") != ret.pop().as_deref() {
                        return Err("while statement must end with a conditional".into());
                    }
                    if ret != type_stack {
                        return Err("While block can not change the stack".into());
                    }
                }
            }
        }
        Ok(type_stack)
    }

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
    Fn(String, Vec<Atom>, Vec<String>, Vec<String>),
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
                Token::Larger => Atom::Op(Op::Greater),
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

        let mut params = vec![];
        match lex.peek() {
            Some((Token::KwWith, _)) => {
                lex.next();
                while let Some((Token::Ident, span)) = lex.peek() {
                    params.push(prog[span.clone()].to_string());
                    lex.next();
                }
            }
            _ => (),
        };

        let mut ret = vec![];
        match lex.peek() {
            Some((Token::KwReturns, _)) => {
                lex.next();
                while let Some((Token::Ident, span)) = lex.peek() {
                    ret.push(prog[span.clone()].to_string());
                    lex.next();
                }
            }
            _ => (),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn prog_to_sema(prog: &'static str) -> Sema {
        let mut lexer = crate::token::lex(prog);
        let ast = Ast::parse(prog, &mut lexer);
        Sema::from_ast(ast)
    }

    #[test]
    fn basic_parse() {
        let sema = prog_to_sema(
            r#"
            fn main begin
                1 2 print print
            end
        "#,
        );
        assert_eq!(
            format!("{:?}", sema.functions()),
            r#"{"main": Code([Num(1), Num(2), Fn("print"), Fn("print")], [], [])}"#,
        );
    }

    #[test]
    fn if_type_check() {
        let sema = prog_to_sema(
            r#"
            fn good begin
                true if 1 else 2 end print
            end
            fn bad begin
                true if 1 else 2 end
            end
        "#,
        );
        assert!(sema.type_check("good").is_ok());
        assert!(sema.type_check("bad").is_err());
    }

    #[test]
    fn while_type_check() {
        let sema = prog_to_sema(
            r#"
            fn good begin
                5 dup 0 > while
                    dup print
                    1 -
                    dup 0 >
                end
                print
            end
            fn bad begin
                5 dup 0 > while
                    dup print
                    1 -
                    0 >
                end
            end
        "#,
        );
        assert!(sema.type_check("good").is_ok());
        assert!(sema.type_check("bad").is_err());
    }

    #[test]
    fn function_type_check() {
        let sema = prog_to_sema(
            r#"
            fn good begin
                3 2 max print
            end
            fn bad begin
                3 max print
            end
            fn max with int int returns int begin
                over over > if
                    drop
                else
                    rot drop
                end
            end
        "#,
        );
        assert!(sema.type_check("good").is_ok());
        assert!(sema.type_check("bad").is_err());
        assert!(sema.type_check("max").is_ok());
    }
}
