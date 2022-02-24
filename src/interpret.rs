use std::collections::HashMap;

use crate::sema::{Atom, Sema, Function, Op};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    U64(u64),
    Bool(bool),
}

pub struct Interpreter {
    sema: Sema,
}

impl Interpreter {
    pub fn new(mut sema: Sema) -> Self {
        let mut builtins = HashMap::new();
        builtins
            .entry("printc".to_string())
            .or_insert(Function::Builtin(Box::new(|stack| {
                match stack.pop().unwrap() {
                    Value::U64(v) => {
                        println!("{}", char::try_from(u32::try_from(v).unwrap()).unwrap());
                    },
                    _ => panic!("Can only print integers as chars"),
                }
            })));
        builtins
            .entry("print".to_string())
            .or_insert(Function::Builtin(Box::new(|stack| {
                match stack.pop().unwrap() {
                    Value::U64(v) => println!("{}", v),
                    Value::Bool(b) => println!("{}", b),
                }
            })));
        builtins
            .entry("dup".to_string())
            .or_insert(Function::Builtin(Box::new(|stack| {
                let top = stack.pop().unwrap();
                stack.push(top);
                stack.push(top);
            })));
        builtins
            .entry("over".to_string())
            .or_insert(Function::Builtin(Box::new(|stack| {
                let fst = stack.pop().unwrap();
                let snd = stack.pop().unwrap();
                stack.push(snd);
                stack.push(fst);
                stack.push(snd);
            })));
        builtins
            .entry("drop".to_string())
            .or_insert(Function::Builtin(Box::new(|stack| {
                stack.pop().unwrap();
            })));
        builtins
            .entry("rot".to_string())
            .or_insert(Function::Builtin(Box::new(|stack| {
                let fst = stack.pop().unwrap();
                let snd = stack.pop().unwrap();
                stack.push(fst);
                stack.push(snd);
            })));
         
        sema.insert(builtins);

        Interpreter { sema }
    }

    pub fn evaluate(&self) -> Result<(), String> {
        let mut stack = vec![];
        self.eval_fn("main", &mut stack)?;
        if stack.is_empty() {
            Ok(())
        } else {
            Err(format!("Too many return values: `{stack:?}`"))
        }
    }

    pub fn eval_fn(&self, func: &str, stack: &mut Vec<Value>) -> Result<(), String> {
        match self.sema.functions().get(func).unwrap() {
            Function::Code(block, ..) => {
                self.eval(block, stack)?;
            }
            Function::Builtin(f, ..) => f(stack),
        }
        Ok(())
    }

    pub fn eval(&self, block: &Vec<Atom>, stack: &mut Vec<Value>) -> Result<(), String> {
        for atom in block {
            match atom {
                Atom::Num(v) => stack.push(Value::U64(*v)),
                Atom::Bool(b) => stack.push(Value::Bool(*b)),
                Atom::Fn(func) => self.eval_fn(&func, stack)?,
                Atom::Op(Op::Plus) => {
                    if let Some(Value::U64(fst)) = stack.pop() {
                        if let Some(Value::U64(snd)) = stack.pop() {
                            let res = snd.wrapping_add(fst);
                            stack.push(Value::U64(res));
                        } else {
                            return Err("Can only add integers".into());
                        }
                    } else {
                        return Err("Can only add integers".into());
                    };
                }
                Atom::Op(Op::Minus) => {
                    if let Some(Value::U64(fst)) = stack.pop() {
                        if let Some(Value::U64(snd)) = stack.pop() {
                            let res = snd.wrapping_sub(fst);
                            stack.push(Value::U64(res));
                        } else {
                            return Err("Can only add integers".into());
                        }
                    } else {
                        return Err("Can only add integers".into());
                    };
                }
                Atom::Op(Op::Equal) => {
                    if let Some(fst) = stack.pop() {
                        if let Some(snd) = stack.pop() {
                            let res = fst == snd;
                            stack.push(Value::Bool(res));
                        } else {
                            return Err("Need two values to compare, found 1".into());
                        }
                    } else {
                        return Err("Need two values to compare, found 0".into());
                    };
                }
                Atom::Op(Op::Less) => {
                    if let Some(Value::U64(fst)) = stack.pop() {
                        if let Some(Value::U64(snd)) = stack.pop() {
                            let res = snd < fst;
                            stack.push(Value::Bool(res));
                        } else {
                            return Err("Can only compare integers".into());
                        }
                    } else {
                        return Err("Can only compare integers".into());
                    };
                }
                Atom::Op(Op::Greater) => {
                    if let Some(Value::U64(fst)) = stack.pop() {
                        if let Some(Value::U64(snd)) = stack.pop() {
                            let res = snd > fst;
                            stack.push(Value::Bool(res));
                        } else {
                            return Err("Can only compare integers".into());
                        }
                    } else {
                        return Err("Can only compare integers".into());
                    };
                }
                Atom::While(block) => {
                    loop {
                        if let Some(Value::Bool(cond)) = stack.pop() {
                            if cond {
                                self.eval(block, stack)?;
                            } else {
                                break;
                            }
                        } else {
                            return Err("Condition must be integer (hehe)".into());
                        }
                    }
                }
                Atom::If(then, otherwise) => {
                    if let Some(Value::Bool(cond)) = stack.pop() {
                        if cond {
                            self.eval(then, stack)?;
                        } else if let Some(otherwise) = otherwise {
                            self.eval(otherwise, stack)?;
                        }
                    } else {
                        return Err("Condition must be integer (hehe)".into());
                    }
                }
            }
        }
        Ok(())
    }
}
