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
    Assign,
    Statements,
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
            BinaryOpType::Assign => {
                panic!("Unsupported yet.");
            }
            BinaryOpType::Statements => {
                panic!("Unsupported yet.");
            }
        }
        assembly.push(String::from("  push rax"));
    }
}

#[derive(Debug)]
pub struct VariableNode {
    pub name: String,
}
impl VariableNode {
    pub fn new(name: &str) -> Self {
        VariableNode {
            name: name.to_string(),
        }
    }
    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        panic!("Not implemented yet.");
    }
}

#[derive(Debug)]
pub struct ExprError {
    pub message: &'static str,
    pub pos: (usize, usize),
}
impl ExprError {
    pub fn new(message: &'static str, pos: (usize, usize)) -> Self {
        ExprError { message, pos }
    }
}

type InternalExprError = Option<ExprError>;

#[derive(Debug)]
pub enum Expr {
    ConstInt(ConstIntNode),
    Variable(VariableNode),
    BinaryOp(Box<BinaryOpNode>),
}
impl Expr {
    pub fn from(tokens: &[Token]) -> Result<Self, ExprError> {
        let (node, _) = Self::parse_as_stmts(tokens)?;
        Ok(node)
    }

    pub fn gen_assembly(&self, assembly: &mut Vec<String>) {
        match self {
            Expr::ConstInt(node) => node.gen_assembly(assembly),
            Expr::Variable(node) => node.gen_assembly(assembly),
            Expr::BinaryOp(node) => node.gen_assembly(assembly),
        }
    }

    fn parse_as_stmts(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_stmt(tokens)?;
        loop {
            if unused_tokens.is_empty() {
                break;
            }
            let (stmt_node, tmp_unused_tokens) = Self::parse_as_stmt(unused_tokens)?;
            node = Expr::BinaryOp(Box::new(BinaryOpNode::new(
                BinaryOpType::Statements,
                node,
                stmt_node,
            )));
            unused_tokens = tmp_unused_tokens;
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_stmt(tokens: &[Token]) -> Result<(Self, &[Token]), ExprError> {
        match Self::parse_as_expr(tokens) {
            Ok((node, unused_tokens)) => {
                if unused_tokens.is_empty() {
                    Err(ExprError::new("; is expected.", tokens.last().unwrap().pos))
                } else if unused_tokens[0].is_endofstmt() {
                    Ok((node, &unused_tokens[1..]))
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
            Err(e) => match e {
                Some(e) => Err(e),
                None => Err(ExprError::new("An expression is expected.", (0, 0))),
            },
        }
        // let (node, unused_tokens) = Self::parse_as_expr(tokens)?;
        // if !unused_tokens.is_empty() && !unused_tokens[0].is_endofstmt() {
        //     Err(ExprError::new(
        //         "; is expected.",
        //         (unused_tokens[0].pos.0, unused_tokens[0].pos.1 + 1),
        //     ))
        // } else {
        //     Ok((node, unused_tokens))
        // }
    }

    fn parse_as_expr(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        Self::parse_as_assign(tokens)
    }

    fn parse_as_assign(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let (mut node, unused_tokens) = Self::parse_as_eqnterm(tokens)?;
        if unused_tokens.is_empty() || !unused_tokens[0].is_assign_op() {
            Ok((node, unused_tokens))
        } else {
            let (expr_node, tmp_unused_tokens) =
                Self::parse_as_expr(&unused_tokens[1..]).map_err(|e| match e {
                    Some(e) => Some(e),
                    None => Some(ExprError::new(
                        "An expression is expected.",
                        (unused_tokens[0].pos.0, unused_tokens[0].pos.1 + 1),
                    )),
                })?;
            node = Expr::BinaryOp(Box::new(BinaryOpNode::new(
                BinaryOpType::Assign,
                node,
                expr_node,
            )));
            Ok((node, tmp_unused_tokens))
        }
    }

    fn parse_as_eqnterm(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_relterm(tokens)?;
        loop {
            if unused_tokens.len() == 0 || !unused_tokens[0].is_eqn_op() {
                break;
            }
            let (relterm_node, tmp_unused_tokens) = Self::parse_as_relterm(&unused_tokens[1..])
                .map_err(|e| match e {
                    Some(e) => Some(e),
                    None => Some(ExprError::new(
                        "An expression is expected.",
                        (unused_tokens[0].pos.0, unused_tokens[0].pos.1 + 1),
                    )),
                })?;
            node = Expr::BinaryOp(Box::new(match unused_tokens[0].token_type {
                TokenType::DoubleEq => BinaryOpNode::new(BinaryOpType::Eql, node, relterm_node),
                TokenType::ExclamEq | _ => BinaryOpNode::new(BinaryOpType::Neq, node, relterm_node),
            }));
            unused_tokens = tmp_unused_tokens;
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_relterm(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_addterm(tokens)?;
        loop {
            if unused_tokens.len() == 0 || !unused_tokens[0].is_rel_op() {
                break;
            }
            let (add_term, tmp_unused_tokens) = Self::parse_as_addterm(&unused_tokens[1..])
                .map_err(|e| match e {
                    Some(e) => Some(e),
                    None => Some(ExprError::new(
                        "An expression is expected.",
                        (unused_tokens[0].pos.0, unused_tokens[0].pos.1 + 1),
                    )),
                })?;
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

    fn parse_as_addterm(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_multerm(tokens)?;
        loop {
            if unused_tokens.len() == 0 || !unused_tokens[0].is_addsub_op() {
                break;
            }
            let (multerm_node, tmp_unused_tokens) = Self::parse_as_multerm(&unused_tokens[1..])
                .map_err(|e| match e {
                    Some(e) => Some(e),
                    None => Some(ExprError::new(
                        "An expression is expected.",
                        (unused_tokens[0].pos.0, unused_tokens[0].pos.1 + 1),
                    )),
                })?;
            node = Expr::BinaryOp(Box::new(match unused_tokens[0].token_type {
                TokenType::Plus => BinaryOpNode::new(BinaryOpType::Add, node, multerm_node),
                TokenType::Minus | _ => BinaryOpNode::new(BinaryOpType::Sub, node, multerm_node),
            }));
            unused_tokens = tmp_unused_tokens;
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_multerm(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let (mut node, mut unused_tokens) = Self::parse_as_unary(tokens)?;
        loop {
            if unused_tokens.len() == 0 || !unused_tokens[0].is_muldiv_op() {
                break;
            }
            let (unary_node, tmp_unused_tokens) = Self::parse_as_unary(&unused_tokens[1..])
                .map_err(|e| match e {
                    Some(e) => Some(e),
                    None => Some(ExprError::new(
                        "An expression is expected.",
                        (unused_tokens[0].pos.0, unused_tokens[0].pos.1 + 1),
                    )),
                })?;
            node = Expr::BinaryOp(Box::new(match unused_tokens[0].token_type {
                TokenType::Asterisk => BinaryOpNode::new(BinaryOpType::Mul, node, unary_node),
                TokenType::Slash | _ => BinaryOpNode::new(BinaryOpType::Div, node, unary_node),
            }));
            unused_tokens = tmp_unused_tokens;
        }
        Ok((node, unused_tokens))
    }

    fn parse_as_unary(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let token = &tokens.get(0).ok_or(None)?;
        match token.token_type {
            TokenType::Plus => Self::parse_as_factor(&tokens[1..]),
            TokenType::Minus => {
                let (expr, unused_tokens) =
                    Self::parse_as_factor(&tokens[1..]).map_err(|e| match e {
                        Some(e) => Some(e),
                        None => Some(ExprError::new(
                            "An expression is expected.",
                            (token.pos.0, token.pos.1 + 1),
                        )),
                    })?;
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

    fn parse_as_factor(tokens: &[Token]) -> Result<(Self, &[Token]), InternalExprError> {
        let token = &tokens.get(0).ok_or(None)?;
        match token.token_type {
            TokenType::Num(n) => Ok((Expr::ConstInt(ConstIntNode::new(n as i64)), &tokens[1..])),
            TokenType::Ident(ref idnt) => {
                Ok((Expr::Variable(VariableNode::new(idnt)), &tokens[1..]))
            }
            TokenType::LBrckt => {
                let (expr, unused_tokens) = Self::parse_as_expr(&tokens[1..])?;
                let next_token = &unused_tokens
                    .get(0)
                    .ok_or(ExprError::new("Corresponding ) is missing.", token.pos))?;
                match next_token.token_type {
                    TokenType::RBrckt => Ok((expr, &unused_tokens[1..])),
                    _ => Err(Some(ExprError::new("Invalid operator.", next_token.pos))),
                }
            }
            _ => Err(Some(ExprError::new(
                "An expression is expected.",
                token.pos,
            ))),
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
