#[derive(Debug)]
pub struct ConstIntNode {
    val: i64,
}
impl ConstIntNode {
    pub fn new(val: i64) -> Self {
        ConstIntNode { val }
    }
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

use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct ExprError {
    pub message: &'static str,
    pub pos: usize,
}
impl ExprError {
    pub fn new(message: &'static str, pos: usize) -> Self {
        ExprError { message, pos }
    }
}

impl Expr {
    pub fn gen(tokens: &[Token]) -> Result<Self, ExprError> {
        let (node, unused_tokens) = Self::parse_as_expr(tokens)?;
        if unused_tokens.is_empty() {
            Ok(node)
        } else {
            match unused_tokens[0].token_type {
                TokenType::Unknown => {
                    Err(ExprError::new("Unknown Operator.", unused_tokens[0].pos))
                }
                _ => Err(ExprError::new("Operator is missing.", unused_tokens[0].pos)),
            }
        }
    }

    fn parse_as_expr(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_term(tokens)?;

        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_addsub_op() {
                match Self::parse_as_term(&unused_tokens[1..]) {
                    Ok((num_expr, tmp_unused_tokens)) => {
                        node =
                            Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                        unused_tokens = tmp_unused_tokens;
                    }
                    Err(e) => {
                        // return Err(ExprError::new(
                        //     "A number was not found after the operator.",
                        //     unused_tokens[0].pos + 1,
                        // ))
                        return Err(e);
                    }
                }
            } else {
                break; // return Err(ExprError::new("Unknown Operator.", unused_tokens[0].pos));
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_term(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_factor(tokens)?;

        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_muldiv_op() {
                match Self::parse_as_factor(&unused_tokens[1..]) {
                    Ok((num_expr, tmp_unused_tokens)) => {
                        node =
                            Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                        unused_tokens = tmp_unused_tokens;
                    }
                    Err(e) => {
                        // return Err(ExprError::new(
                        //     "A number was not found after the operator.",
                        //     unused_tokens[0].pos + 1,
                        // ))
                        return Err(e);
                    }
                }
            } else {
                break;
                // return Err(ExprError::new("Unknown Operator.", unused_tokens[0].pos));
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_factor(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let token = &tokens.get(0).ok_or(ExprError::new("", 0))?;
        match token.token_type {
            TokenType::Num(n) => Ok((Expr::ConstInt(ConstIntNode::new(n as i64)), &tokens[1..])),
            TokenType::LBracket => {
                let (expr, unused_tokens) = Self::parse_as_expr(&tokens[1..])?;
                let next_token = &unused_tokens
                    .get(0)
                    .ok_or(ExprError::new("Pair ) is missing.", token.pos))?;
                match next_token.token_type {
                    TokenType::RBracket => Ok((expr, &unused_tokens[1..])),
                    _ => Err(ExprError::new("Invalid operator.", next_token.pos)),
                }
            }
            _ => Err(ExprError::new("The token is not a number.", token.pos)),
        }
    }

    // fn parse_as_num(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
    //     let token = &tokens
    //         .get(0)
    //         .ok_or(ExprError::new("A number token was not found.", 0))?;
    //     match token.token_type {
    //         TokenType::Num(n) => Ok((Expr::ConstInt(ConstIntNode::new(n as i64)), &tokens[1..])),
    //         _ => Err(ExprError::new("The token is not a number.", token.pos)),
    //     }
    // }
}
