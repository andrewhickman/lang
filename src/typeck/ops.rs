use ast;

use super::Ty;

pub fn ty_of(op: ast::BinaryOp, left: Ty, right: Ty) -> Result<Ty, String> {
    use ast::BinaryOp::*;

    match op {
        // int . int -> int
        Mul | Div | Rem | Add | Sub | AssignMul | AssignDiv | AssignRem | AssignAdd | AssignSub => {
            if left == Ty::Int && right == Ty::Int {
                return Ok(Ty::Int);
            }
        }
        // byte . int -> byte
        Shr | Shl | AssignShr | AssignShl => {
            if left == Ty::Byte && right == Ty::Int {
                return Ok(Ty::Byte);
            }
        }
        // T . T -> bool
        Eq | Neq | Gt | Ge | Lt | Le => {
            if left == right {
                return Ok(Ty::Bool);
            }
        }
        // byte . byte -> byte
        BitAnd | BitXor | BitOr | AssignBitAnd | AssignBitXor | AssignBitOr => {
            if left == Ty::Byte && right == Ty::Byte {
                return Ok(Ty::Byte);
            }
        }
        // bool . bool -> bool
        And | Or => {
            if left == Ty::Bool && right == Ty::Bool {
                return Ok(Ty::Bool);
            }
        }
        // T . T -> T
        Assign => {
            if left == right {
                return Ok(left);
            }
        }
    }
    Err(format!("bad operand types {:?} and {:?} for operator {}", left, right, op))
}