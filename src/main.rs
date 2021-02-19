pub mod token;

use token::{TokenIter, TokenType};

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
    let mut token_iter = TokenIter::new(arg.as_str());

    println!(".intel_syntax noprefix");
    println!(".globl _main");
    println!("_main:");

    if let Some(first_token) = token_iter.next() {
        match first_token.token_type {
            TokenType::Num(num) => println!("  mov rax, {}", num),
            _ => {}
        }
    } else {
        eprintln!("Empty source.");
        std::process::exit(1);
    }

    while let Some(op_token) = token_iter.next() {
        match op_token.token_type {
            TokenType::Plus => {
                if let Some(num_token) = token_iter.next() {
                    if let Some(num) = num_token.as_num() {
                        println!("  add rax, {}", num);
                    } else {
                        error(
                            arg.as_str(),
                            "The token after operator must be a number.",
                            num_token.pos + 1,
                        );
                    }
                } else {
                    error(
                        arg.as_str(),
                        "This token must be an operator.",
                        op_token.pos,
                    );
                }
            }
            TokenType::Minus => {
                if let Some(num_token) = token_iter.next() {
                    if let Some(num) = num_token.as_num() {
                        println!("  sub rax, {}", num);
                    } else {
                        error(
                            arg.as_str(),
                            "The token after operator must be a number.",
                            num_token.pos + 1,
                        );
                    }
                } else {
                    error(
                        arg.as_str(),
                        "This token must be an operator.",
                        op_token.pos,
                    );
                }
            }
            _ => error(arg.as_str(), "Unexpected operator.", op_token.pos),
        }
    }

    println!("  ret");
}
