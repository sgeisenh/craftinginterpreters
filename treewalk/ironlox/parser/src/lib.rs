use std::rc::Rc;

use error::LoxError;
use scanner::{Token, TokenType};
use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
}

impl From<&Token<'_>> for BinOp {
    fn from(token: &Token) -> Self {
        match token.r#type {
            TokenType::Plus => BinOp::Plus,
            TokenType::Minus => BinOp::Minus,
            TokenType::Star => BinOp::Star,
            TokenType::Slash => BinOp::Slash,
            TokenType::BangEqual => BinOp::BangEqual,
            TokenType::EqualEqual => BinOp::EqualEqual,
            TokenType::Greater => BinOp::Greater,
            TokenType::GreaterEqual => BinOp::GreaterEqual,
            TokenType::Less => BinOp::Less,
            TokenType::LessEqual => BinOp::LessEqual,
            TokenType::And => BinOp::And,
            TokenType::Or => BinOp::Or,
            _ => panic!("Invalid binary operator token"),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinOp,
    pub right: Box<Expr>,
}

impl From<BinaryExpr> for Expr {
    fn from(expr: BinaryExpr) -> Self {
        Self::Binary(expr)
    }
}

#[derive(Debug, Serialize)]
pub enum LiteralExpr {
    True,
    False,
    Nil,
    Number(f64),
    String(Rc<String>),
}

impl From<LiteralExpr> for Expr {
    fn from(expr: LiteralExpr) -> Self {
        Self::Literal(expr)
    }
}

#[derive(Debug, Serialize)]
pub enum UnOp {
    Negative,
    Bang,
}

impl From<&Token<'_>> for UnOp {
    fn from(token: &Token) -> Self {
        match token.r#type {
            TokenType::Minus => UnOp::Negative,
            TokenType::Bang => UnOp::Bang,
            _ => panic!("Invalid binary operator token"),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct UnaryExpr {
    pub operator: UnOp,
    pub right: Box<Expr>,
}

impl From<UnaryExpr> for Expr {
    fn from(expr: UnaryExpr) -> Self {
        Self::Unary(expr)
    }
}

#[derive(Debug, Serialize)]
pub struct VariableExpr {
    pub name: String,
}

impl From<VariableExpr> for Expr {
    fn from(expr: VariableExpr) -> Self {
        Self::Variable(expr)
    }
}

#[derive(Debug, Serialize)]
pub struct GroupingExpr(pub Box<Expr>);

impl From<GroupingExpr> for Expr {
    fn from(expr: GroupingExpr) -> Self {
        Self::Grouping(expr)
    }
}

#[derive(Debug, Serialize)]
pub struct AssignExpr {
    pub name: String,
    pub expr: Box<Expr>,
}

impl From<AssignExpr> for Expr {
    fn from(expr: AssignExpr) -> Self {
        Self::Assign(expr)
    }
}

#[derive(Debug, Serialize)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
    Assign(AssignExpr),
}

#[derive(Debug, Serialize)]
pub struct ExprStmt {
    pub expr: Expr,
}

impl From<ExprStmt> for Stmt {
    fn from(stmt: ExprStmt) -> Self {
        Self::Expression(stmt)
    }
}

#[derive(Debug, Serialize)]
pub struct PrintStmt {
    pub expr: Expr,
}

impl From<PrintStmt> for Stmt {
    fn from(stmt: PrintStmt) -> Self {
        Self::Print(stmt)
    }
}

#[derive(Debug, Serialize)]
pub struct VarStmt {
    pub name: String,
    pub initializer: Expr,
}

impl From<VarStmt> for Stmt {
    fn from(stmt: VarStmt) -> Self {
        Self::Var(stmt)
    }
}

#[derive(Debug, Serialize)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl From<BlockStmt> for Stmt {
    fn from(stmt: BlockStmt) -> Self {
        Self::Block(stmt)
    }
}

#[derive(Debug, Serialize)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl From<IfStmt> for Stmt {
    fn from(stmt: IfStmt) -> Self {
        Self::If(stmt)
    }
}

#[derive(Debug, Serialize)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl From<WhileStmt> for Stmt {
    fn from(stmt: WhileStmt) -> Self {
        Self::While(stmt)
    }
}

#[derive(Debug, Serialize)]
pub enum Stmt {
    Expression(ExprStmt),
    Print(PrintStmt),
    Var(VarStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
}

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    previous: Option<&'a Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Parser {
            tokens,
            previous: None,
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let TokenType::Semicolon = self.previous.unwrap().r#type {
                return;
            }

            match self.peek().r#type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn peek(&self) -> &'a Token<'a> {
        &self.tokens[0]
    }

    fn is_at_end(&self) -> bool {
        self.tokens.is_empty() || self.peek().r#type == TokenType::Eof
    }

    fn advance(&mut self) -> Option<&'a Token<'a>> {
        self.previous = self.tokens.get(0);
        if !self.is_at_end() {
            self.tokens = &self.tokens[1..];
        }
        self.previous
    }

    fn check(&self, typ: &TokenType<'static>) -> bool {
        if let Some(Token {
            r#type: token_type, ..
        }) = self.tokens.get(0)
        {
            token_type == typ
        } else {
            false
        }
    }

    fn mtch(&mut self, types: &[TokenType<'static>]) -> bool {
        if types.iter().any(|typ| self.check(typ)) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn declaration(&mut self) -> Result<Stmt, LoxError> {
        let result = if self.mtch(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        result
    }

    fn var_declaration(&mut self) -> Result<Stmt, LoxError> {
        let Token { r#type, .. } = self.peek();
        let name = match r#type {
            TokenType::Identifier(name) => String::from(*name),
            _ => return Err(LoxError::new(0, "Expect variable name.")),
        };
        self.advance();

        let Token { r#type, .. } = self.peek();
        let initializer = if *r#type == TokenType::Equal {
            self.advance();
            self.expression()?
        } else {
            LiteralExpr::Nil.into()
        };
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(VarStmt { name, initializer }.into())
    }

    fn statement(&mut self) -> Result<Stmt, LoxError> {
        match self.peek().r#type {
            TokenType::For => {
                self.advance();
                self.for_statement()
            }
            TokenType::While => {
                self.advance();
                self.while_statement()
            }
            TokenType::If => {
                self.advance();
                self.if_statement()
            }
            TokenType::Print => {
                self.advance();
                self.print_statement()
            }
            TokenType::LeftBrace => {
                self.advance();
                let statements = self.block()?;
                Ok(BlockStmt { statements }.into())
            }
            _ => self.expression_statement(),
        }
    }

    fn for_statement(&mut self) -> Result<Stmt, LoxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        let initializer = match self.peek().r#type {
            TokenType::Semicolon => {
                self.advance();
                None
            }
            TokenType::Var => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => Some(self.expression_statement()?),
        };
        let condition = match self.peek().r#type {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;
        let increment = match self.peek().r#type {
            TokenType::RightParen => None,
            _ => Some(self.expression()?),
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let body = self.statement()?;
        let body = match increment {
            Some(expr) => BlockStmt {
                statements: vec![body, ExprStmt { expr }.into()],
            }
            .into(),
            None => body,
        };
        let condition = condition.unwrap_or(LiteralExpr::True.into());
        let body = WhileStmt {
            condition,
            body: Box::new(body),
        }
        .into();
        let body = match initializer {
            Some(initializer) => BlockStmt {
                statements: vec![initializer, body],
            }
            .into(),
            None => body,
        };

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, LoxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;

        let body = Box::new(self.statement()?);
        Ok(WhileStmt { condition, body }.into())
    }

    fn if_statement(&mut self) -> Result<Stmt, LoxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.mtch(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(IfStmt {
            condition,
            then_branch,
            else_branch,
        }
        .into())
    }

    fn print_statement(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(PrintStmt { expr }.into())
    }

    fn block(&mut self) -> Result<Vec<Stmt>, LoxError> {
        let mut statements = Vec::new();
        while !self.check(&TokenType::RightBrace) {
            statements.push(self.declaration()?);
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(ExprStmt { expr }.into())
    }

    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, LoxError> {
        let expr = self.or()?;
        if self.mtch(&[TokenType::Equal]) {
            let value = self.assignment()?;
            return match expr {
                Expr::Variable(expr) => Ok(AssignExpr {
                    name: expr.name,
                    expr: Box::new(value),
                }
                .into()),
                _ => Err(LoxError::new(0, "Invalid assignment target.")),
            };
        }
        Ok(expr)
    }

    fn binops(
        &mut self,
        op_types: &[TokenType<'static>],
        next_level: impl Fn(&mut Self) -> Result<Expr, LoxError>,
    ) -> Result<Expr, LoxError> {
        let mut expr = next_level(self)?;
        while self.mtch(op_types) {
            expr = BinaryExpr {
                left: Box::new(expr),
                operator: BinOp::from(self.previous.unwrap()),
                right: Box::new(next_level(self)?),
            }
            .into();
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, LoxError> {
        self.binops(&[TokenType::Or], Self::and)
    }

    fn and(&mut self) -> Result<Expr, LoxError> {
        self.binops(&[TokenType::And], Self::equality)
    }

    fn equality(&mut self) -> Result<Expr, LoxError> {
        self.binops(
            &[TokenType::EqualEqual, TokenType::BangEqual],
            Self::comparison,
        )
    }

    fn comparison(&mut self) -> Result<Expr, LoxError> {
        self.binops(
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            Self::term,
        )
    }

    fn term(&mut self) -> Result<Expr, LoxError> {
        self.binops(&[TokenType::Minus, TokenType::Plus], Self::factor)
    }

    fn factor(&mut self) -> Result<Expr, LoxError> {
        self.binops(&[TokenType::Slash, TokenType::Star], Self::unary)
    }

    fn unary(&mut self) -> Result<Expr, LoxError> {
        if self.mtch(&[TokenType::Bang, TokenType::Minus]) {
            return Ok(UnaryExpr {
                operator: UnOp::from(self.previous.unwrap()),
                right: Box::new(self.unary()?),
            }
            .into());
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, LoxError> {
        let Token { r#type, line } = self.peek();
        let result = match r#type {
            TokenType::False => Ok(LiteralExpr::False.into()),
            TokenType::True => Ok(LiteralExpr::True.into()),
            TokenType::Nil => Ok(LiteralExpr::Nil.into()),
            TokenType::Number(f) => Ok(LiteralExpr::Number(*f).into()),
            TokenType::String(s) => Ok(LiteralExpr::String(Rc::new(String::from(*s))).into()),
            TokenType::Identifier(name) => Ok(VariableExpr {
                name: String::from(*name),
            }
            .into()),
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                return Ok(GroupingExpr(Box::new(expr)).into());
            }
            _ => Err(LoxError::new(*line, "Expect expression.")),
        };
        if result.is_ok() {
            self.advance();
        }
        result
    }

    fn consume(
        &mut self,
        token_type: TokenType<'static>,
        msg: &str,
    ) -> Result<&Token<'a>, LoxError> {
        let front @ Token { r#type, line } = self.peek();
        if token_type == *r#type {
            self.advance();
            return Ok(front);
        }
        Err(LoxError::new(*line, msg))
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt, LoxError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() {
            None
        } else {
            let decl = self.declaration();
            if decl.is_err() {
                self.synchronize();
            }
            Some(decl)
        }
    }
}

#[cfg(test)]
mod tests {}
