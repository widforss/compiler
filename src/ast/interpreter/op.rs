use super::{util, IntprtError, ErrorKind, Ast, BinOp, Expr, Literal, Span, UnOp, State};

use Literal::*;

pub fn intprt_binop<'a>(
    call: BinOp,
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    use BinOp::*;

    let literal = match call {
        Pow => pow_(left, right, span, ast, state),
        Mult => mult(left, right, span, ast, state),
        Rem => rem(left, right, span, ast, state),
        IntDiv => int_div(left, right, span, ast, state),
        Div => div(left, right, span, ast, state),
        Add => add(left, right, span, ast, state),
        Sub => sub(left, right, span, ast, state),
        Lt => lt(left, right, ast, state),
        Gt => gt(left, right, ast, state),
        Leq => leq(left, right, ast, state),
        Geq => geq(left, right, ast, state),
        Eq => eq(left, right, ast, state),
        Neq => neq(left, right, ast, state),
        And => and(left, right, ast, state),
        Or => or(left, right, ast, state),
    };
    Ok(literal?)
}

pub fn intprt_unop<'a>(
    call: UnOp,
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    use UnOp::*;

    let literal = match call {
        Sub => un_sub(arg, ast, state),
        Inv => un_inv(arg, ast, state),
    };
    Ok(literal?)
}

fn un_sub<'a>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        Int(val) => Ok(Int(-val)),
        Float(val) => Ok(Float(-val)),
        _ => panic!(),
    }
}

fn un_inv<'a>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        Bool(val) => Ok(Bool(!val)),
        _ => panic!(),
    }
}

fn pow_<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(pow_ii(left, right), span)?;
            Ok(Int(val))
        },
        (Float(left), Float(right)) => Ok(Float(left.powf(right))),
        (Float(left), Int(right)) => Ok(Float(left.powf(right as f64))),
        _ => panic!(),
    }
}

fn mult<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(left.checked_mul(right), span)?;
            Ok(Int(val))
        },
        (Float(left), Float(right)) => Ok(Float(left * right)),
        (Int(left), Float(right)) => Ok(Float(left as f64 * right)),
        (Float(left), Int(right)) => Ok(Float(left * right as f64)),
        _ => panic!(),
    }
}

fn rem<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            check_zero(Int(right), span)?;
            let val = check(left.checked_rem(right), span)?;
            Ok(Int(val))
        },
        (Float(left), Float(right)) => Ok(Float(left % right)),
        (Int(left), Float(right)) => Ok(Float(left as f64 % right)),
        (Float(left), Int(right)) => Ok(Float(left % right as f64)),
        _ => panic!(),
    }
}

fn int_div<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    match div(left, right, span, ast, state)? {
        Int(val) => Ok(Int(val)),
        Float(val) => Ok(Float(val.floor())),
        _ => panic!(),
    }
}

fn div<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            check_zero(Int(right), span)?;
            let val = check(left.checked_div(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left / right)),
        (Int(left), Float(right)) => Ok(Float(left as f64 / right)),
        (Float(left), Int(right)) => Ok(Float(left / right as f64)),
        _ => panic!(),
    }
}

fn add<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(left.checked_add(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left + right)),
        (Int(left), Float(right)) => Ok(Float(left as f64 + right)),
        (Float(left), Int(right)) => Ok(Float(left + right as f64)),
        _ => panic!(),
    }
}

fn sub<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(left.checked_sub(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left - right)),
        (Int(left), Float(right)) => Ok(Float(left as f64 - right)),
        (Float(left), Int(right)) => Ok(Float(left - right as f64)),
        _ => panic!(),
    }
}

fn lt<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => Ok(Bool(left < right)),
        (Float(left), Float(right)) => Ok(Bool(left < right)),
        _ => panic!(),
    }
}

fn gt<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => Ok(Bool(left > right)),
        (Float(left), Float(right)) => Ok(Bool(left > right)),
        _ => panic!(),
    }
}

fn leq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    if let Bool(val) = gt(left, right, ast, state)? {
        Ok(Bool(!val))
    } else {
        panic!();
    }
}

fn geq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    if let Bool(val) = lt(left, right, ast, state)? {
        Ok(Bool(!val))
    } else {
        panic!();
    }
}

fn eq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Bool(left), Bool(right)) => Ok(Bool(left == right)),
        (Int(left), Int(right)) => Ok(Bool(left == right)),
        (Float(left), Float(right)) => Ok(Bool(left == right)),
        _ => panic!(),
    }
}

fn neq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    if let Bool(val) = eq(left, right, ast, state)? {
        Ok(Bool(!val))
    } else {
        panic!();
    }
}

fn and<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    match util::intprt_expr(left, ast, state)? {
        Bool(false) => Ok(Bool(false)),
        Bool(true) => {
            if let Bool(bool) = util::intprt_expr(right, ast, state)? {
                Ok(Bool(bool))
            } else {
                panic!();
            }
        }
        _ => panic!(),
    }
}

fn or<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    match util::intprt_expr(left, ast, state)? {
        Bool(true) => Ok(Bool(true)),
        Bool(false) => {
            if let Bool(bool) = util::intprt_expr(right, ast, state)? {
                Ok(Bool(bool))
            } else {
                panic!();
            }
        }
        _ => panic!(),
    }
}

fn check_zero<'a>(val: Literal, span: Span<'a>) -> Result<Literal, IntprtError<'a>> {
    match val {
        Int(0) => Err(IntprtError::new(Some(span), ErrorKind::DivisionByZero)),
        Int(_) => Ok(val),
        _ => panic!(),
    }
}

fn pow_ii(mut base: i64, mut exp: i64) -> Option<i64> {
    if exp < 0 {
        return Some(0);
    }
    let mut acc: i64 = 1;

    while exp > 1 {
        if (exp & 1) == 1 {
            acc = acc.checked_mul(base)?;
        }
        exp /= 2;
        base = base.checked_mul(base)?;
    }

    if exp == 1 {
        acc = acc.checked_mul(base)?;
    }

    Some(acc)
}

fn check<'a, T>(val: Option<T>, span: Span<'a>) -> Result<T, IntprtError> {
    match val {
        Some(val) => Ok(val),
        None => Err(IntprtError::new(Some(span), ErrorKind::Overflow)),
    }
}
