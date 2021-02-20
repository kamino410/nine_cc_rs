pub mod ast;
pub mod token;

use ast::Expr;
use token::{Token, TokenIter};

fn main() {
    if std::env::args().len() != 2 {
        panic!("Invalid args");
    }

    let arg = std::env::args().nth(1).unwrap();
    let tokens = TokenIter::new(arg.as_str()).collect::<Vec<Token>>();

    match Expr::from(&tokens) {
        Err(e) => {
            eprintln!("[Error]");
            eprintln!("    {}", arg);
            eprintln!("    {: <2$}^ {}", "", e.message, e.pos);
        }
        Ok(expr) => {
            let mut assembly: Vec<String> = vec![];
            assembly.push(String::from(".intel_syntax noprefix"));
            assembly.push(String::from(".globl _main"));
            assembly.push(String::from("_main:"));
            expr.gen_assembly(&mut assembly);
            assembly.push(String::from("  pop rax"));
            assembly.push(String::from("  ret"));

            for line in assembly {
                println!("{}", line);
            }
        }
    }
}
