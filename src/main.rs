pub mod ast;
pub mod token;

use ast::Expr;
use token::{Token, TokenIter, TokenType};

fn error<'a>(line: &'a str, message: &'static str, pos: usize) {
    eprintln!("[Error]");
    eprintln!("    {}", line);
    eprintln!("    {: <2$}^ {}", "", message, pos);
    std::process::exit(1);
}

fn main() {
    if std::env::args().len() != 2 {
        panic!("Invalid args");
    }

    let arg = std::env::args().nth(1).unwrap();
    let tokens = TokenIter::new(arg.as_str()).collect::<Vec<Token>>();

    // println!(".intel_syntax noprefix");
    // println!(".globl _main");
    // println!("_main:");

    println!("{:?}", Expr::gen(&tokens));

    // println!("  ret");
}
