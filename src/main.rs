mod interpret;
mod sema;
mod token;

use owo_colors::OwoColorize;

use crate::interpret::Interpreter;
use sema::{Ast, Sema};

fn main() {
    const PROG: &'static str = r#"
        fn main begin
            3 2 max print
        end
        fn max with int int returns int begin
            over over > if
                drop
            else
                rot drop
            end
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
    match sema.type_check("main") {
        Ok(_) => println!("{}", "type checked `max`!".green()),
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
