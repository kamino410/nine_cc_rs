use crate::ast::BinaryOpType;

#[derive(PartialEq, Clone, Copy)]
pub enum TokenType {
    Num(usize),
    Plus,
    Minus,
    Asterisk,
    Slash,
    LBracket,
    RBracket,
    Unknown,
}

#[derive(PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

impl Token {
    pub fn as_num(&self) -> Option<usize> {
        match self.token_type {
            TokenType::Num(n) => Some(n),
            _ => None,
        }
    }
    pub fn as_addsub_op(&self) -> Option<BinaryOpType> {
        match self.token_type {
            TokenType::Plus => Some(BinaryOpType::Add),
            TokenType::Minus => Some(BinaryOpType::Sub),
            _ => None,
        }
    }
    pub fn as_muldiv_op(&self) -> Option<BinaryOpType> {
        match self.token_type {
            TokenType::Asterisk => Some(BinaryOpType::Mul),
            TokenType::Slash => Some(BinaryOpType::Div),
            _ => None,
        }
    }
}

pub struct TokenIter<'a> {
    s: &'a str,
    pos: usize,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // skip spaces
        let first_non_space_idx = self.s.find(|c| c != ' ').unwrap_or(self.s.len());
        self.s = self.s.trim_start();
        self.pos += first_non_space_idx;

        if self.s.is_empty() {
            return None;
        }

        // integer
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

        // 1 character operator
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
            "*" => Some(Token {
                token_type: TokenType::Asterisk,
                pos: self.pos - 1,
            }),
            "/" => Some(Token {
                token_type: TokenType::Slash,
                pos: self.pos - 1,
            }),
            "(" => Some(Token {
                token_type: TokenType::LBracket,
                pos: self.pos - 1,
            }),
            ")" => Some(Token {
                token_type: TokenType::RBracket,
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
    pub fn new(s: &'a str) -> TokenIter<'a> {
        TokenIter { s: s, pos: 0 }
    }
}

#[test]
fn tokenize_test() {
    let raw_code = String::from("1 *(4-31) /2 + 5");
    let res = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let expected = vec![
        Token {
            token_type: TokenType::Num(1),
            pos: 0,
        },
        Token {
            token_type: TokenType::Asterisk,
            pos: 2,
        },
        Token {
            token_type: TokenType::LBracket,
            pos: 3,
        },
        Token {
            token_type: TokenType::Num(4),
            pos: 4,
        },
        Token {
            token_type: TokenType::Minus,
            pos: 5,
        },
        Token {
            token_type: TokenType::Num(31),
            pos: 6,
        },
        Token {
            token_type: TokenType::RBracket,
            pos: 8,
        },
        Token {
            token_type: TokenType::Slash,
            pos: 10,
        },
        Token {
            token_type: TokenType::Num(2),
            pos: 11,
        },
        Token {
            token_type: TokenType::Plus,
            pos: 13,
        },
        Token {
            token_type: TokenType::Num(5),
            pos: 15,
        },
    ];
    let matching = expected
        .iter()
        .zip(res.iter())
        .filter(|&(a, b)| a == b)
        .count();
    assert!(res.len() == matching && expected.len() == matching);
}
