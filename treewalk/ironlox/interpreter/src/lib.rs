use std::{collections::HashMap, fmt::Display, rc::Rc};

use error::LoxError;
use parser::{
    AssignExpr, BinOp, BinaryExpr, BlockStmt, Expr, ExprStmt, GroupingExpr, LiteralExpr, Parser,
    PrintStmt, Stmt, UnaryExpr, VarStmt, VariableExpr,
};
use scanner::Scanner;
use serde::Serialize;

#[derive(Debug, PartialEq, Serialize)]
enum LoxValue {
    Nil,
    Bool(bool),
    String(Rc<String>),
    Number(f64),
}

impl From<&LoxValue> for bool {
    fn from(value: &LoxValue) -> Self {
        !matches!(value, LoxValue::Nil | LoxValue::Bool(false))
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, Serialize)]
struct Environment(Vec<HashMap<String, Rc<LoxValue>>>);

impl Environment {
    fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    fn define(&mut self, name: String, value: Rc<LoxValue>) {
        self.0.last_mut().unwrap().insert(name, value);
    }

    fn assign(&mut self, name: &str, value: Rc<LoxValue>) -> Result<(), LoxError> {
        match self.0.last_mut().unwrap().get_mut(name) {
            Some(v) => {
                *v = value;
                Ok(())
            }
            None => Err(LoxError::new(0, format!("Undefined variable '{}'.", name))),
        }
    }

    fn get(&self, name: &str) -> Result<Rc<LoxValue>, LoxError> {
        match self.0.iter().rev().find_map(|ctx| ctx.get(name)) {
            Some(v) => Ok(v.clone()),
            None => Err(LoxError::new(0, format!("Undefined variable '{}'.", name))),
        }
    }

    fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.0.pop();
    }
}

#[derive(Serialize)]
pub struct Interpreter {
    environment: Environment,
}

trait Evalable {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError>;
}

impl Evalable for BinaryExpr {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        let Self {
            left,
            operator,
            right,
        } = self;
        let left = &*left.eval(interpreter)?;
        let right = &*right.eval(interpreter)?;
        Ok(Rc::new(match operator {
            BinOp::Plus => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Number(left + right),
                (LoxValue::String(left), LoxValue::String(right)) => {
                    LoxValue::String(Rc::new((**left).clone() + right))
                }
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::Minus => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Number(left - right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::Star => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Number(left * right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::Slash => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Number(left / right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::BangEqual => LoxValue::Bool(left != right),
            BinOp::EqualEqual => LoxValue::Bool(left == right),
            BinOp::Greater => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Bool(left > right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::GreaterEqual => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Bool(left >= right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::Less => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Bool(left < right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::LessEqual => match (left, right) {
                (LoxValue::Number(left), LoxValue::Number(right)) => LoxValue::Bool(left <= right),
                _ => return Err(LoxError::new(0, "Type error")),
            },
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        }))
    }
}

impl Evalable for UnaryExpr {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        let Self { operator, right } = self;
        let right = &*right.eval(interpreter)?;
        match operator {
            parser::UnOp::Negative => match right {
                LoxValue::Number(n) => Ok(Rc::new(LoxValue::Number(-n))),
                _ => Err(LoxError::new(0, "Type error")),
            },
            parser::UnOp::Bang => Ok(Rc::new(LoxValue::Bool(!bool::from(right)))),
        }
    }
}

impl Evalable for GroupingExpr {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        self.0.eval(interpreter)
    }
}

impl Evalable for LiteralExpr {
    fn eval(&self, _: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        Ok(Rc::new(match self {
            LiteralExpr::True => LoxValue::Bool(true),
            LiteralExpr::False => LoxValue::Bool(false),
            LiteralExpr::Nil => LoxValue::Nil,
            LiteralExpr::Number(n) => LoxValue::Number(*n),
            LiteralExpr::String(s) => LoxValue::String(s.clone()),
        }))
    }
}

impl Evalable for VariableExpr {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        interpreter.environment.get(&self.name)
    }
}

impl Evalable for AssignExpr {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        let value = self.expr.eval(interpreter)?;
        interpreter.environment.assign(&self.name, value.clone())?;
        Ok(value)
    }
}

impl Evalable for Expr {
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Rc<LoxValue>, LoxError> {
        match self {
            Expr::Binary(e) => e.eval(interpreter),
            Expr::Grouping(e) => e.eval(interpreter),
            Expr::Literal(e) => e.eval(interpreter),
            Expr::Unary(e) => e.eval(interpreter),
            Expr::Variable(e) => e.eval(interpreter),
            Expr::Assign(e) => e.eval(interpreter),
        }
    }
}

trait Execable {
    fn exec(&self, interpreter: &mut Interpreter) -> Result<(), LoxError>;
}

impl Execable for ExprStmt {
    fn exec(&self, interpreter: &mut Interpreter) -> Result<(), LoxError> {
        self.expr.eval(interpreter)?;
        Ok(())
    }
}

impl Execable for PrintStmt {
    fn exec(&self, interpreter: &mut Interpreter) -> Result<(), LoxError> {
        let value = self.expr.eval(interpreter)?;
        println!("{}", value);
        Ok(())
    }
}

impl Execable for VarStmt {
    fn exec(&self, interpreter: &mut Interpreter) -> Result<(), LoxError> {
        let value = self.initializer.eval(interpreter)?;
        interpreter.environment.define(self.name.clone(), value);
        Ok(())
    }
}

impl Execable for BlockStmt {
    fn exec(&self, interpreter: &mut Interpreter) -> Result<(), LoxError> {
        interpreter.environment.push_scope();
        let result = self
            .statements
            .iter()
            .try_for_each(|stmt| stmt.exec(interpreter));
        interpreter.environment.pop_scope();
        result
    }
}

impl Execable for Stmt {
    fn exec(&self, interpreter: &mut Interpreter) -> Result<(), LoxError> {
        match self {
            Stmt::Expression(s) => s.exec(interpreter),
            Stmt::Print(s) => s.exec(interpreter),
            Stmt::Var(s) => s.exec(interpreter),
            Stmt::Block(s) => s.exec(interpreter),
        }
    }
}

impl Interpreter {
    pub fn evaluate(&mut self, expr: Expr) -> Result<impl Display, LoxError> {
        expr.eval(self)
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }

    pub fn run(&mut self, input: &str) -> Result<(), LoxError> {
        let tokens = Scanner::new(input).collect::<Result<Vec<_>, _>>()?;
        let ast = Parser::new(&tokens).collect::<Result<Vec<_>, _>>()?;
        ast.iter().try_for_each(|stmt| stmt.exec(self))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {}
