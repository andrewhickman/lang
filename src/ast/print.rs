use std::fmt::*;

use super::*;

impl<'a> Display for Ast<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for statement in &self.main.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl<'a> Display for Scope<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "{{")?;
        for statement in &self.statements {
            writeln!(f, "    {}", statement)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Statement::Expression(ref expr) => write!(f, "{};", expr),
            Statement::Declaration(ref decl) => write!(f, "{};", decl),
            Statement::Scope(ref scope) => write!(f, "{}", scope),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.kind {
            ExprKind::Term(ref term) => write!(f, "{}", term),
            ExprKind::Unary { ref op, ref arg } => {
                use self::UnaryOp::*;
                match *op {
                    Not | Plus | Minus | PreIncr | PreDecr => write!(f, "({} {})", op, arg),
                    PostIncr | PostDecr => write!(f, "({} {})", arg, op),
                }
            }
            ExprKind::Binary { ref op, ref args } => {
                let (ref left, ref right) = **args;
                write!(f, "({} {} {})", left, op, right)
            },
        }
    }
}

impl<'a> Display for Decl<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let {}: {:?}", self.name, self.ty)?;
        if let Some(ref expr) = self.expr {
            write!(f, " = {}", expr)?;
        }
        Ok(())
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
            BinaryOp::AssignMul => write!(f, "*="),
            BinaryOp::AssignDiv => write!(f, "/="), 
            BinaryOp::AssignRem => write!(f, "%="), 
            BinaryOp::AssignAdd => write!(f, "+="), 
            BinaryOp::AssignSub => write!(f, "-="), 
            BinaryOp::AssignShr => write!(f, ">>="), 
            BinaryOp::AssignShl => write!(f, "<<="), 
            BinaryOp::AssignBitAnd => write!(f, "&="), 
            BinaryOp::AssignBitXor => write!(f, "^="), 
            BinaryOp::AssignBitOr => write!(f, "|="),
        }
    }
}
