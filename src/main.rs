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

    match Expr::gen(&tokens) {
        Err(e) => {
            eprintln!("[Error]");
            eprintln!("    {}", arg);
            eprintln!("    {: <2$}^ {}", "", e.message, e.pos);
        }
        Ok(expr) => {
            println!(".intel_syntax noprefix");
            println!(".globl _main");
            println!("_main:");

            let mut assembly: Vec<String> = vec![];
            expr.gen_assembly(&mut assembly);

            for line in assembly {
                println!("{}", line);
            }

            println!("  pop rax");
            println!("  ret");
        }
    }
}
