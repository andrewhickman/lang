use ast;

pub fn ty_of(op: ast::BinaryOp, left: ast::Ty, right: ast::Ty) -> Result<ast::Ty, String> {
    use ast::BinaryOp::*;

    match op {
        // int . int -> int
        Mul | Div | Rem | Add | Sub | AssignMul | AssignDiv | AssignRem | AssignAdd | AssignSub => {
            if left == ast::Ty::Int && right == ast::Ty::Int {
                return Ok(ast::Ty::Int);
            }
        }
        // byte . int -> byte
        Shr | Shl | AssignShr | AssignShl => {
            if left == ast::Ty::Byte && right == ast::Ty::Int {
                return Ok(ast::Ty::Byte);
            }
        }
        // T . T -> bool
        Eq | Neq | Gt | Ge | Lt | Le => {
            if left == right {
                return Ok(ast::Ty::Bool);
            }
        }
        // byte . byte -> byte
        BitAnd | BitXor | BitOr | AssignBitAnd | AssignBitXor | AssignBitOr => {
            if left == ast::Ty::Byte && right == ast::Ty::Byte {
                return Ok(ast::Ty::Byte);
            }
        }
        // bool . bool -> bool
        And | Or => {
            if left == ast::Ty::Bool && right == ast::Ty::Bool {
                return Ok(ast::Ty::Bool);
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