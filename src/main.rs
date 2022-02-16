use std::collections::HashMap;

use logos::{Logos, SpannedIter};

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token("fn")]
    KwFn,
    #[token("begin")]
    KwBegin,
    #[token("end")]
    KwEnd,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[regex("[0-9]+")]
    Num,
    #[regex("[_a-zA-Z]+")]
    Ident,
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
enum Atom {
    Num(u64),
    Fn(String),
    Op(Op),
}

#[derive(Debug, Clone, Copy)]
enum Value {
    U64(u64),
}

#[derive(Debug, Clone)]
enum Ast {
    Prog(Vec<Ast>),
    Fn(String, Vec<Atom>),
}

struct Sema {
    functions: HashMap<String, Block>,
}

impl Sema {
    fn from_ast(ast: Ast) -> Self {
        fn extract_funcs(ast: Ast, functions: &mut HashMap<String, Block>) {
            match ast {
                Ast::Prog(funcs) => funcs
                    .into_iter()
                    .for_each(|ast| extract_funcs(ast, functions)),
                Ast::Fn(func, atoms) => {
                    functions.insert(func, Block(atoms));
                }
            }
        }
        let mut functions = HashMap::new();
        extract_funcs(ast, &mut functions);
        Sema { functions }
    }
}

#[derive(Debug, Clone)]
struct Block(pub Vec<Atom>);

impl Block {
    fn eval(&self, stack: &mut Vec<Value>) {
        for atom in &self.0 {
            match atom {
                Atom::Num(v) => stack.push(Value::U64(*v)),
                Atom::Fn(func) => unimplemented!("Calling: {}", func),
                Atom::Op(Op::Plus) => {
                    if let Some(Value::U64(fst)) = stack.pop() {
                        if let Some(Value::U64(snd)) = stack.pop() {
                            let res = fst + snd;
                            stack.push(Value::U64(res));
                        } else {
                            panic!("Can't add non-integers")
                        }
                    } else {
                        panic!("Can't add non-integers")
                    };
                }
                Atom::Op(Op::Minus) => {
                    if let Some(Value::U64(fst)) = stack.pop() {
                        if let Some(Value::U64(snd)) = stack.pop() {
                            let res = fst - snd;
                            stack.push(Value::U64(res));
                        } else {
                            panic!("Can't add non-integers")
                        }
                    } else {
                        panic!("Can't add non-integers")
                    };
                }
            }
        }
    }
}

fn parse_block<'src>(prog: &'src str, lex: &mut SpannedIter<'src, Token>) -> Vec<Atom> {
    let mut atoms = vec![];
    while let Some((token, span)) = lex.next() {
        atoms.push(match token {
            Token::Plus => Atom::Op(Op::Plus),
            Token::Minus => Atom::Op(Op::Minus),
            Token::Num => Atom::Num(prog[span].parse().unwrap()),
            Token::Ident => Atom::Fn(prog[span].to_string()),
            Token::KwEnd => return atoms,
            _ => unimplemented!("{:?}", token),
        });
    }
    atoms
}

fn parse_fn<'src>(prog: &'src str, lex: &mut SpannedIter<'src, Token>) -> Ast {
    let ident = match lex.next() {
        Some((Token::Ident, span)) => prog[span].to_string(),
        t => unimplemented!("{:?}", t),
    };

    match lex.next() {
        Some((Token::KwBegin, _)) => (),
        t => unimplemented!("{:?}", t),
    }

    Ast::Fn(ident, parse_block(prog, lex))
}

fn parse<'src>(prog: &'src str, lex: &mut SpannedIter<'src, Token>) -> Ast {
    let mut items = vec![];
    while let Some((token, _)) = lex.next() {
        match token {
            Token::KwFn => items.push(parse_fn(prog, lex)),
            _ => unimplemented!("{token:?}"),
        };
    }
    Ast::Prog(items)
}

fn main() {
    const PROG: &'static str = r#"
        fn foo begin
            1 1 bar
        end
        fn bar begin
            +
        end
    "#;
    let lexer = Token::lexer(PROG);
    let ast = parse(PROG, &mut lexer.spanned());

    println!("{ast:?}");
    let sema = Sema::from_ast(ast);
    let foo = sema.functions.get("foo").unwrap();
    println!("{foo:?}");
    let bar = sema.functions.get("bar").unwrap();
    println!("{bar:?}");
    let mut stack = vec![Value::U64(1), Value::U64(1)];
    bar.eval(&mut stack);
    println!("{stack:?}");
}
