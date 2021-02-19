#[derive(Debug)]
pub struct ConstIntNode {
    val: i64,
}
impl ConstIntNode {
    pub fn new(val: i64) -> Self {
        ConstIntNode { val }
    }
    // pub fn eval(&self) -> i64 {
    //     self.val
    // }
    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        assembly.push(String::from(format!("  push {}", self.val)));
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
}
#[derive(Debug)]
pub struct BinaryOpNode {
    op_type: BinaryOpType,
    l_expr: Expr,
    r_expr: Expr,
}
impl BinaryOpNode {
    pub fn new(op_type: BinaryOpType, l_expr: Expr, r_expr: Expr) -> Self {
        BinaryOpNode {
            op_type,
            l_expr,
            r_expr,
        }
    }
    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        self.l_expr.gen_assembly(assembly);
        self.r_expr.gen_assembly(assembly);
        assembly.push(String::from("  pop rdi"));
        assembly.push(String::from("  pop rax"));
        match self.op_type {
            BinaryOpType::Add => assembly.push(String::from("  add rax, rdi")),
            BinaryOpType::Sub => assembly.push(String::from("  sub rax, rdi")),
            BinaryOpType::Mul => assembly.push(String::from("  imul rax, rdi")),
            BinaryOpType::Div => {
                assembly.push(String::from("  cqo"));
                assembly.push(String::from("  idiv rdi"));
            }
        }
        assembly.push(String::from("  push rax"));
    }
    // pub fn eval(&self) -> i64 {
    //     let l_val = self.l_expr.eval();
    //     let r_val = self.r_expr.eval();
    //     match self.op_type {
    //         BinaryOpType::Add => l_val + r_val,
    //         BinaryOpType::Sub => l_val - r_val,
    //         BinaryOpType::Mul => l_val * r_val,
    //         BinaryOpType::Div => l_val / r_val,
    //     }
    // }
}

#[derive(Debug)]
pub enum Expr {
    ConstInt(ConstIntNode),
    BinaryOp(Box<BinaryOpNode>),
}
impl Expr {
    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        match self {
            Expr::ConstInt(node) => node.gen_assembly(assembly),
            Expr::BinaryOp(node) => node.gen_assembly(assembly),
        }
    }
}

// impl Expr {
//     pub fn eval(&self) -> i64 {
//         match self {
//             Expr::ConstInt(node) => node.eval(),
//             Expr::BinaryOp(node) => node.eval(),
//         }
//     }
// }

// #[test]
// fn binary_op_test1() {
//     let expr = BinaryOpNode::new(
//         BinaryOpType::Mul,
//         Expr::ConstInt(ConstIntNode::new(8)),
//         Expr::BinaryOp(Box::new(BinaryOpNode::new(
//             BinaryOpType::Add,
//             Expr::ConstInt(ConstIntNode::new(10)),
//             Expr::ConstInt(ConstIntNode::new(3)),
//         ))),
//     );
//     assert_eq!(expr.eval(), 8i64 * (10i64 + 3i64));
// }
//
// #[test]
// fn binary_op_test2() {
//     let expr = BinaryOpNode::new(
//         BinaryOpType::Div,
//         Expr::BinaryOp(Box::new(BinaryOpNode::new(
//             BinaryOpType::Sub,
//             Expr::ConstInt(ConstIntNode::new(0)),
//             Expr::ConstInt(ConstIntNode::new(-6)),
//         ))),
//         Expr::ConstInt(ConstIntNode::new(-8)),
//     );
//     assert_eq!(expr.eval(), (0i64 - (-6i64)) / -8i64);
// }

use crate::token::{Token, TokenIter, TokenType};

#[derive(Debug)]
pub struct ExprError {
    message: &'static str,
    pos: usize,
}
impl ExprError {
    pub fn new(message: &'static str, pos: usize) -> Self {
        ExprError { message, pos }
    }
}

impl Expr {
    pub fn gen(tokens: &[Token]) -> Result<Self, ExprError> {
        let (node, _unused_tokens) = Self::parse_as_expr(tokens)?;
        Ok(node)
    }
    fn parse_as_expr(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_num(tokens)?;

        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_op() {
                match Self::parse_as_num(&unused_tokens[1..]) {
                    Ok((num_expr, tmp_unused_tokens)) => {
                        node =
                            Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                        unused_tokens = tmp_unused_tokens;
                    }
                    Err(e) => return Err(e),
                }
            } else {
                return Err(ExprError::new("", 0));
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_num(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let token = &tokens
            .get(0)
            .ok_or(ExprError::new("A number token was not found.", 0))?;
        match token.token_type {
            TokenType::Num(n) => Ok((Expr::ConstInt(ConstIntNode::new(n as i64)), &tokens[1..])),
            _ => Err(ExprError::new("The token is not a number.", token.pos)),
        }
    }
}

// #[test]
// fn token2ast_test() {
//     let tokens = vec![Token {
//         token_type: TokenType::Num(5),
//         pos: 0,
//     }];
//     let res = Expr::gen(&tokens).ok().unwrap();
//     assert_eq!(res.eval(), 5i64);
// }

// #[test]
// fn token_eval_test() {
//     let raw_code = String::from("1 + 4-31  +1");
//     let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
//     let res = Expr::gen(&tokens).ok().unwrap().eval();
//     assert_eq!(res, -25i64);
// }
