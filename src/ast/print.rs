use std::fmt::*;

use super::*;

impl<'a> Display for Ast<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Statement::Expression(ref expr) => write!(f, "{};", expr),
            Statement::Declaration(ref decl) => write!(f, "{};", decl),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Expr::Term(ref term) => write!(f, "{}", term),
            Expr::Unary { ref op, ref arg } => {
                use self::UnaryOp::*;
                match *op {
                    Not | Plus | Minus | PreIncr | PreDecr => write!(f, "({} {})", op, arg),
                    PostIncr | PostDecr => write!(f, "({} {})", arg, op),
                }
            }
            Expr::Binary { ref op, ref args } => {
                let (ref left, ref right) = **args;
                write!(f, "({} {} {})", left, op, right)
            },
        }
    }
}

impl<'a> Display for Decl<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.expr {
            Some(ref expr) => write!(f, "let {} = {}", self.name, expr),
            None => write!(f, "let {}", self.name),
        }
    }
}

impl<'a> Display for Term<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Term::Literal(num) => write!(f, "{}", num),
            Term::Ident(ident) => write!(f, "{}", ident),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::PostIncr | UnaryOp::PreIncr => write!(f, "++"),
            UnaryOp::PostDecr | UnaryOp::PreDecr => write!(f, "--"),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Neq => write!(f, "!="),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::Assign => write!(f, "="),
        }
    }
}
