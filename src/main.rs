mod interpret;
mod sema;
mod token;

use owo_colors::OwoColorize;

use crate::interpret::Interpreter;
use sema::{Ast, Sema};

fn main() {
    const PROG: &'static str = r#"
        fn main begin
            true if 1 else 2 end print
        end
    "#;
    let mut lexer = token::lex(PROG);
    let ast = Ast::parse(PROG, &mut lexer);

    println!("ast: {ast:?}");
    let sema = Sema::from_ast(ast);
    match sema.type_check("main") {
        Ok(_) => println!("{}", "type checked `main`!".green()),
        Err(e) => {
            println!("{}\n{e}", "Did not type check:".red());
            return;
        }
    }

    let interpreter = Interpreter::new(sema);
    if let Err(err) = interpreter.evaluate() {
        eprintln!("{}", err.red());
    }
}
