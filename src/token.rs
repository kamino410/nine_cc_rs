#[derive(Debug, PartialEq)]
pub enum TokenType {
    Ident(String),
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
    SemiColon,
    LineFeed,
    Equal,
    Unknown,
}

#[derive(PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: (usize, usize),
}

impl Token {
    pub fn new(token_type: TokenType, pos: (usize, usize)) -> Self {
        Token { token_type, pos }
    }
    pub fn as_num(&self) -> Option<usize> {
        match self.token_type {
            TokenType::Num(n) => Some(n),
            _ => None,
        }
    }
    pub fn is_endofstmt(&self) -> bool {
        match self.token_type {
            TokenType::SemiColon => true,
            _ => false,
        }
    }
    pub fn is_assign_op(&self) -> bool {
        match self.token_type {
            TokenType::Equal => true,
            _ => false,
        }
    }
    pub fn is_addsub_op(&self) -> bool {
        match self.token_type {
            TokenType::Plus | TokenType::Minus => true,
            _ => false,
        }
    }
    pub fn is_muldiv_op(&self) -> bool {
        match self.token_type {
            TokenType::Asterisk | TokenType::Slash => true,
            _ => false,
        }
    }
    pub fn is_eqn_op(&self) -> bool {
        match self.token_type {
            TokenType::DoubleEq | TokenType::ExclamEq => true,
            _ => false,
        }
    }
    pub fn is_rel_op(&self) -> bool {
        match self.token_type {
            TokenType::LAnglBrckt
            | TokenType::RAnglBrckt
            | TokenType::LAnglBrcktEq
            | TokenType::RAnglBrcktEq => true,
            _ => false,
        }
    }
}

pub struct TokenIter<'a> {
    s: &'a str,
    pos: (usize, usize),
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // skip spaces
        let first_non_space_idx = self
            .s
            .find(|c| !char::is_whitespace(c))
            .unwrap_or(self.s.len());
        self.s = self.s.trim_start();
        self.pos.1 += first_non_space_idx;

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
            self.pos.1 += first_non_num_idx;
            return Some(Token::new(
                TokenType::Num(s_num.parse::<usize>().unwrap()),
                front_pos,
            ));
        }

        let cur_offset = self.pos.1;

        // 2 character operator
        if self.s.len() >= 2 {
            if let Some(token) = match &self.s[..2] {
                "==" => Some(Token::new(TokenType::DoubleEq, (self.pos.0, cur_offset))),
                "!=" => Some(Token::new(TokenType::ExclamEq, (self.pos.0, cur_offset))),
                "<=" => Some(Token::new(
                    TokenType::LAnglBrcktEq,
                    (self.pos.0, cur_offset),
                )),
                ">=" => Some(Token::new(
                    TokenType::RAnglBrcktEq,
                    (self.pos.0, cur_offset),
                )),
                _ => None,
            } {
                self.s = &self.s[2..];
                self.pos.1 += 2;
                return Some(token);
            }
        }

        // 1 character operator
        if let Some(token) = match &self.s[..1] {
            "+" => Some(Token::new(TokenType::Plus, (self.pos.0, cur_offset))),
            "-" => Some(Token::new(TokenType::Minus, (self.pos.0, cur_offset))),
            "*" => Some(Token::new(TokenType::Asterisk, (self.pos.0, cur_offset))),
            "/" => Some(Token::new(TokenType::Slash, (self.pos.0, cur_offset))),
            "(" => Some(Token::new(TokenType::LBrckt, (self.pos.0, cur_offset))),
            ")" => Some(Token::new(TokenType::RBrckt, (self.pos.0, cur_offset))),
            "<" => Some(Token::new(TokenType::LAnglBrckt, (self.pos.0, cur_offset))),
            ">" => Some(Token::new(TokenType::RAnglBrckt, (self.pos.0, cur_offset))),
            ";" => Some(Token::new(TokenType::SemiColon, (self.pos.0, cur_offset))),
            "=" => Some(Token::new(TokenType::Equal, (self.pos.0, cur_offset))),
            _ => None,
        } {
            self.s = &self.s[1..];
            self.pos.1 += 1;
            return Some(token);
        } else if &self.s[..1] == "\n" {
            self.s = &self.s[1..];
            self.pos.0 += 1;
            self.pos.1 = 0;
            return Some(Token::new(TokenType::LineFeed, (self.pos.0, cur_offset)));
        }

        // variable
        let first_non_alphabet_idx = self
            .s
            .find(|c| !char::is_alphabetic(c))
            .unwrap_or(self.s.len());
        if first_non_alphabet_idx > 0 {
            let (s_ident, s_unused) = self.s.split_at(first_non_alphabet_idx);
            self.s = s_unused;
            self.pos.1 += first_non_space_idx;
            Some(Token::new(
                TokenType::Ident(s_ident.to_string()),
                (self.pos.0, cur_offset),
            ))
        } else {
            self.s = &self.s[1..];
            self.pos.1 += 1;
            Some(Token::new(TokenType::Unknown, (self.pos.0, cur_offset)))
        }
    }
}

impl<'a> TokenIter<'a> {
    pub fn new(s: &'a str) -> TokenIter<'a> {
        TokenIter { s: s, pos: (0, 0) }
    }
}

#[test]
fn tokenize_test1() {
    let raw_code = String::from("(1<4)!=-3*(1==1)");
    let res = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let expected = vec![
        Token::new(TokenType::LBrckt, (0, 0)),
        Token::new(TokenType::Num(1), (0, 1)),
        Token::new(TokenType::LAnglBrckt, (0, 2)),
        Token::new(TokenType::Num(4), (0, 3)),
        Token::new(TokenType::RBrckt, (0, 4)),
        Token::new(TokenType::ExclamEq, (0, 5)),
        Token::new(TokenType::Minus, (0, 7)),
        Token::new(TokenType::Num(3), (0, 8)),
        Token::new(TokenType::Asterisk, (0, 9)),
        Token::new(TokenType::LBrckt, (0, 10)),
        Token::new(TokenType::Num(1), (0, 11)),
        Token::new(TokenType::DoubleEq, (0, 12)),
        Token::new(TokenType::Num(1), (0, 14)),
        Token::new(TokenType::RBrckt, (0, 15)),
    ];
    let matching = expected
        .iter()
        .zip(res.iter())
        .filter(|&(a, b)| a == b)
        .count();
    assert!(res.len() == matching && expected.len() == matching);
}
#[test]
fn tokenize_test2() {
    let raw_code = String::from("1 *(4-31) /2 + 5");
    let res = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let expected = vec![
        Token::new(TokenType::Num(1), (0, 0)),
        Token::new(TokenType::Asterisk, (0, 2)),
        Token::new(TokenType::LBrckt, (0, 3)),
        Token::new(TokenType::Num(4), (0, 4)),
        Token::new(TokenType::Minus, (0, 5)),
        Token::new(TokenType::Num(31), (0, 6)),
        Token::new(TokenType::RBrckt, (0, 8)),
        Token::new(TokenType::Slash, (0, 10)),
        Token::new(TokenType::Num(2), (0, 11)),
        Token::new(TokenType::Plus, (0, 13)),
        Token::new(TokenType::Num(5), (0, 15)),
    ];
    let matching = expected
        .iter()
        .zip(res.iter())
        .filter(|&(a, b)| a == b)
        .count();
    assert!(res.len() == matching && expected.len() == matching);
}
