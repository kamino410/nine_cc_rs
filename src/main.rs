#[derive(Debug)]
enum TokenType {
    Num(usize),
    Plus,
    Minus,
    Space,
    Unknown,
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
    pos: usize,
}

impl Token {
    pub fn as_num(&self) -> Option<usize> {
        match self.token_type {
            TokenType::Num(n) => Some(n),
            _ => None,
        }
    }
}

struct TokenIter<'a> {
    s: &'a str,
    pos: usize,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.s.is_empty() {
            return None;
        }

        let first_non_space_idx = self.s.find(|c| c != ' ').unwrap_or(self.s.len());
        self.s = self.s.trim_start();
        self.pos += first_non_space_idx;

        let first_non_num_idx = self
            .s
            .find(|c| !char::is_numeric(c))
            .unwrap_or(self.s.len());
        if first_non_num_idx != 0 {
            let (s_num, s_remains) = self.s.split_at(first_non_num_idx);
            let front_pos = self.pos;
            self.s = s_remains;
            self.pos += first_non_num_idx;
            return Some(Token {
                token_type: TokenType::Num(s_num.parse::<usize>().unwrap()),
                pos: front_pos,
            });
        }

        let (s_op, s_remains) = self.s.split_at(1);
        self.s = s_remains;
        self.pos += 1;
        match s_op {
            "+" => Some(Token {
                token_type: TokenType::Plus,
                pos: self.pos - 1,
            }),
            "-" => Some(Token {
                token_type: TokenType::Minus,
                pos: self.pos - 1,
            }),
            _ => Some(Token {
                token_type: TokenType::Unknown,
                pos: self.pos - 1,
            }),
        }
    }
}

impl<'a> TokenIter<'a> {
    fn new(s: &'a str) -> TokenIter<'a> {
        TokenIter { s: s, pos: 0 }
    }
}

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
