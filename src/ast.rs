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
    Eql,
    Neq,
    Lt,
    Leq,
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
            BinaryOpType::Eql => {
                assembly.push(String::from("  cmp rax, rdi"));
                assembly.push(String::from("  sete al"));
                assembly.push(String::from("  movzx rax, al"));
            }
            BinaryOpType::Neq => {
                assembly.push(String::from("  cmp rax, rdi"));
                assembly.push(String::from("  setne al"));
                assembly.push(String::from("  movzx rax, al"));
            }
            BinaryOpType::Lt => {
                assembly.push(String::from("  cmp rax, rdi"));
                assembly.push(String::from("  setl al"));
                assembly.push(String::from("  movzx rax, al"));
            }
            BinaryOpType::Leq => {
                assembly.push(String::from("  cmp rax, rdi"));
                assembly.push(String::from("  setle al"));
                assembly.push(String::from("  movzx rax, al"));
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
        let (node, unused_tokens) = Self::parse_as_eqnterm(tokens)?;
        if unused_tokens.is_empty() {
            Ok(node)
        } else {
            Err(ExprError::new(
                match unused_tokens[0].token_type {
                    TokenType::RBrckt => "Corresponding ( is missing.",
                    TokenType::Unknown => "Unknown Operator.",
                    _ => "Operator is missing.",
                },
                unused_tokens[0].pos,
            ))
        }
    }

    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        match self {
            Expr::ConstInt(node) => node.gen_assembly(assembly),
            Expr::BinaryOp(node) => node.gen_assembly(assembly),
        }
    }

    fn parse_as_eqnterm(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_relterm(tokens)?;
        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_eqn_op() {
                let (num_expr, tmp_unused_tokens) = Self::parse_as_relterm(&unused_tokens[1..])?;
                node = Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                unused_tokens = tmp_unused_tokens;
            } else {
                break;
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_relterm(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_addterm(tokens)?;
        loop {
            if unused_tokens.len() == 0 || !unused_tokens[0].is_rel_op() {
                break;
            }
            let (add_term, tmp_unused_tokens) = Self::parse_as_addterm(&unused_tokens[1..])?;
            node = Expr::BinaryOp(Box::new(match unused_tokens[0].token_type {
                TokenType::LAnglBrckt => BinaryOpNode::new(BinaryOpType::Lt, node, add_term),
                TokenType::RAnglBrckt => BinaryOpNode::new(BinaryOpType::Lt, add_term, node),
                TokenType::LAnglBrcktEq => BinaryOpNode::new(BinaryOpType::Leq, node, add_term),
                TokenType::RAnglBrcktEq | _ => BinaryOpNode::new(BinaryOpType::Leq, add_term, node),
            }));
            unused_tokens = tmp_unused_tokens;
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_addterm(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_multerm(tokens)?;
        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_addsub_op() {
                let (num_expr, tmp_unused_tokens) = Self::parse_as_multerm(&unused_tokens[1..])?;
                node = Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                unused_tokens = tmp_unused_tokens;
            } else {
                break;
            }
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_multerm(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_unary(tokens)?;
        loop {
            if unused_tokens.len() == 0 {
                break;
            }
            if let Some(op_type) = &unused_tokens[0].as_muldiv_op() {
                let (num_expr, tmp_unused_tokens) = Self::parse_as_unary(&unused_tokens[1..])?;
                node = Expr::BinaryOp(Box::new(BinaryOpNode::new(*op_type, node, num_expr)));
                unused_tokens = tmp_unused_tokens;
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
            TokenType::LBrckt => {
                let (expr, unused_tokens) = Self::parse_as_eqnterm(&tokens[1..])?;
                let next_token = &unused_tokens
                    .get(0)
                    .ok_or(ExprError::new("Corresponding ) is missing.", token.pos))?;
                match next_token.token_type {
                    TokenType::RBrckt => Ok((expr, &unused_tokens[1..])),
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
    let raw_code = String::from("1 *(4<31) /2 + 5");
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
    let _ = Expr::from(&tokens).ok().unwrap();
}
#[test]
fn ast_parse_ok_test4() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("(3<4<5 != 1)==(1 <= 2)");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).ok().unwrap();
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
#[test]
fn ast_parse_err_test3() {
    use crate::token::{Token, TokenIter};
    let raw_code = String::from("124 <!= (3+5)");
    let tokens = TokenIter::new(raw_code.as_str()).collect::<Vec<Token>>();
    let _ = Expr::from(&tokens).err().unwrap();
}
