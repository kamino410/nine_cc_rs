use crate::token::{Token, TokenType};

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
pub struct ExprError {
    pub message: &'static str,
    pub pos: usize,
}
impl ExprError {
    pub fn new(message: &'static str, pos: usize) -> Self {
        ExprError { message, pos }
    }
}

#[derive(Debug)]
pub enum Expr {
    ConstInt(ConstIntNode),
    BinaryOp(Box<BinaryOpNode>),
}
impl Expr {
    pub fn from(tokens: &[Token]) -> Result<Self, ExprError> {
        let (node, unused_tokens) = Self::parse_as_expr(tokens)?;
        if unused_tokens.is_empty() {
            Ok(node)
        } else {
            match unused_tokens[0].token_type {
                TokenType::RBracket => Err(ExprError::new(
                    "Corresponding ( is missing.",
                    unused_tokens[0].pos,
                )),
                TokenType::Unknown => {
                    Err(ExprError::new("Unknown Operator.", unused_tokens[0].pos))
                }
                _ => Err(ExprError::new("Operator is missing.", unused_tokens[0].pos)),
            }
        }
    }

    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        match self {
            Expr::ConstInt(node) => node.gen_assembly(assembly),
            Expr::BinaryOp(node) => node.gen_assembly(assembly),
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
                        return Err(e);
                    }
                }
            } else {
                break;
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_term(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_unary(tokens)?;

        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_muldiv_op() {
                match Self::parse_as_unary(&unused_tokens[1..]) {
                    Ok((num_expr, tmp_unused_tokens)) => {
                        node =
                            Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                        unused_tokens = tmp_unused_tokens;
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            } else {
                break;
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_unary(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let token = &tokens
            .get(0)
            .ok_or(ExprError::new("An expression is expected.", 0))?;
        match token.token_type {
            TokenType::Plus => Self::parse_as_factor(&tokens[1..]),
            TokenType::Minus => {
                let (expr, unused_tokens) = Self::parse_as_factor(&tokens[1..])?;
                let expr = Expr::BinaryOp(Box::new(BinaryOpNode::new(
                    BinaryOpType::Sub,
                    Expr::ConstInt(ConstIntNode::new(0i64)),
                    expr,
                )));
                Ok((expr, unused_tokens))
            }
            _ => Self::parse_as_factor(&tokens),
        }
    }

    fn parse_as_factor(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let token = &tokens
            .get(0)
            .ok_or(ExprError::new("An expression is expected.", 0))?;
        match token.token_type {
            TokenType::Num(n) => Ok((Expr::ConstInt(ConstIntNode::new(n as i64)), &tokens[1..])),
            TokenType::LBracket => {
                let (expr, unused_tokens) = Self::parse_as_expr(&tokens[1..])?;
                let next_token = &unused_tokens
                    .get(0)
                    .ok_or(ExprError::new("Corresponding ) is missing.", token.pos))?;
                match next_token.token_type {
                    TokenType::RBracket => Ok((expr, &unused_tokens[1..])),
                    _ => Err(ExprError::new("Invalid operator.", next_token.pos)),
                }
            }
            _ => Err(ExprError::new("A number is expected.", token.pos)),
        }
    }
}

#[test]
fn ast_parse_ok_test1() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("1 *(4-31) /2 + 5");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).ok().unwrap();
}
#[test]
fn ast_parse_ok_test2() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("124- 513+(334)*9/(3+3)  ");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).ok().unwrap();
}
#[test]
fn ast_parse_ok_test3() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("124- -42");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).err().unwrap();
}
#[test]
fn ast_parse_err_test1() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("124* (3 + 2");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).err().unwrap();
}
#[test]
fn ast_parse_err_test2() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("124* (3 + 2))");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).err().unwrap();
}
