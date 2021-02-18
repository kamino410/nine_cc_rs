enum Token {
    Plus,
    Minus,
    Num(usize),
}

impl Token {
    pub fn expect_num(&self, s: &'static str) -> usize {
        match self {
            Token::Num(n) => *n,
            _ => panic!(s),
        }
    }
}

struct TokenIter<'a> {
    s: &'a str,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.s = self.s.trim_start();
        if self.s.is_empty() {
            return None;
        }

        let first_non_num_idx = self
            .s
            .find(|c| !char::is_numeric(c))
            .unwrap_or(self.s.len());
        if first_non_num_idx != 0 {
            let (s_num, s_remains) = self.s.split_at(first_non_num_idx);
            self.s = s_remains;
            return Some(Token::Num(s_num.parse::<usize>().unwrap()));
        }

        let (s_op, s_remains) = self.s.split_at(1);
        self.s = s_remains;
        if s_op == "+" {
            return Some(Token::Plus);
        }
        if s_op == "-" {
            return Some(Token::Minus);
        }

        panic!("Unknown token")
    }
}

impl<'a> TokenIter<'a> {
    fn new(s: &'a str) -> TokenIter<'a> {
        TokenIter { s: s }
    }
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

    let first_num = token_iter
        .next()
        .expect("Source was empty.")
        .expect_num("The first token was not a number.");
    println!("  mov rax, {}", first_num);

    while let Some(token) = token_iter.next() {
        let num = token_iter
            .next()
            .expect("No token after the operator.")
            .expect_num("The token after the operator was not a number.");
        match token {
            Token::Plus => println!("  add rax, {}", num),
            Token::Minus => println!("  sub rax, {}", num),
            _ => panic!("Unexpected operator"),
        }
    }

    println!("  ret");
}
