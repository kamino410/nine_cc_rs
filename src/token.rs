use crate::ast::BinaryOpType;

#[derive(PartialEq, Clone, Copy)]
pub enum TokenType {
    Num(usize),
    Plus,
    Minus,
    Asterisk,
    Slash,
    LBrckt,
    RBrckt,
    DoubleEq,
    ExclamEq,
    LAnglBrckt,
    RAnglBrckt,
    LAnglBrcktEq,
    RAnglBrcktEq,
    Unknown,
}

#[derive(PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

impl Token {
    pub fn new(token_type: TokenType, pos: usize) -> Self {
        Token { token_type, pos }
    }
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
            return Some(Token::new(
                TokenType::Num(s_num.parse::<usize>().unwrap()),
                front_pos,
            ));
        }

        let cur_pos = self.pos;

        // 2 character operator
        if self.s.len() >= 2 {
            if let Some(token) = match &self.s[..2] {
                "==" => Some(Token::new(TokenType::DoubleEq, cur_pos)),
                "!=" => Some(Token::new(TokenType::ExclamEq, cur_pos)),
                "<=" => Some(Token::new(TokenType::LAnglBrcktEq, cur_pos)),
                ">=" => Some(Token::new(TokenType::RAnglBrcktEq, cur_pos)),
                _ => None,
            } {
                self.s = &self.s[2..];
                self.pos += 2;
                return Some(token);
            }
        }

        // 1 character operator
        let res = match &self.s[..1] {
            "+" => Some(Token::new(TokenType::Plus, cur_pos)),
            "-" => Some(Token::new(TokenType::Minus, cur_pos)),
            "*" => Some(Token::new(TokenType::Asterisk, cur_pos)),
            "/" => Some(Token::new(TokenType::Slash, cur_pos)),
            "(" => Some(Token::new(TokenType::LBrckt, cur_pos)),
            ")" => Some(Token::new(TokenType::RBrckt, cur_pos)),
            "<" => Some(Token::new(TokenType::LAnglBrckt, cur_pos)),
            ">" => Some(Token::new(TokenType::RAnglBrckt, cur_pos)),
            _ => Some(Token::new(TokenType::Unknown, cur_pos)),
        };
        self.s = &self.s[1..];
        self.pos += 1;
        return res;
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
        Token::new(TokenType::Num(1), 0),
        Token::new(TokenType::Asterisk, 2),
        Token::new(TokenType::LBrckt, 3),
        Token::new(TokenType::Num(4), 4),
        Token::new(TokenType::Minus, 5),
        Token::new(TokenType::Num(31), 6),
        Token::new(TokenType::RBrckt, 8),
        Token::new(TokenType::Slash, 10),
        Token::new(TokenType::Num(2), 11),
        Token::new(TokenType::Plus, 13),
        Token::new(TokenType::Num(5), 15),
    ];
    let matching = expected
        .iter()
        .zip(res.iter())
        .filter(|&(a, b)| a == b)
        .count();
    assert!(res.len() == matching && expected.len() == matching);
}
