mod token;
mod sema;
mod interpret;

use owo_colors::OwoColorize;

use sema::{Ast, Sema};
use crate::interpret::Interpreter;

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
//          fn not with bool returns bool begin
//              if false else true end
//          end
    let mut lexer = token::lex(PROG);
    let ast = Ast::parse(PROG, &mut lexer);

    println!("ast: {ast:?}");
    let sema = Sema::from_ast(ast);

    let interpreter = Interpreter::new(sema);
    if let Err(err) = interpreter.evaluate() {
        eprintln!("{}", err.red());
    }
}
