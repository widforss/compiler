use super::{util, Ast, BinOp, ErrorKind, Expr, IntprtError, Literal, Span, State, UnOp, Value};

use Literal::*;

pub fn intprt_binop<'a, 'b>(
    call: BinOp,
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
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

pub fn intprt_unop<'a, 'b>(
    call: UnOp,
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    use UnOp::*;

    let literal = match call {
        Sub => un_sub(arg, ast, state),
        Inv => un_inv(arg, ast, state),
        AsFloat => as_float(arg, ast, state),
        Ref(_) => reference(arg, ast, state),
        Deref => deref(arg, ast, state),
    };
    Ok(literal?)
}

fn un_sub<'a, 'b>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        Int(val) => Ok(Int(-val)),
        Float(val) => Ok(Float(-val)),
        _ => panic!(),
    }
}

fn un_inv<'a, 'b>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        Bool(val) => Ok(Bool(!val)),
        _ => panic!(),
    }
}

fn as_float<'a, 'b>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        Int(val) => Ok(Float(val as f64)),
        _ => panic!(),
    }
}

fn reference<'a, 'b>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    match arg.value {
        Value::Ident(ident) => Ok(Ref(state.ref_var(ident).unwrap())),
        _ => {
            let arg = util::intprt_expr(arg, ast, state)?;
            Ok(Ref(state.ref_literal(arg)))
        }
    }
}

fn deref<'a, 'b>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        Ref(pointer) => Ok(*state.deref(pointer)),
        _ => panic!(),
    }
}

fn pow_<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(pow_ii(left, right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left.powf(right))),
        _ => panic!(),
    }
}

fn mult<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(left.checked_mul(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left * right)),
        _ => panic!(),
    }
}

fn rem<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            check_zero(Int(right), span)?;
            let val = check(left.checked_rem(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left % right)),
        _ => panic!(),
    }
}

fn int_div<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    match div(left, right, span, ast, state)? {
        Int(val) => Ok(Int(val)),
        Float(val) => Ok(Float(val.floor())),
        _ => panic!(),
    }
}

fn div<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            check_zero(Int(right), span)?;
            let val = check(left.checked_div(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left / right)),
        _ => panic!(),
    }
}

fn add<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(left.checked_add(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left + right)),
        _ => panic!(),
    }
}

fn sub<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => {
            let val = check(left.checked_sub(right), span)?;
            Ok(Int(val))
        }
        (Float(left), Float(right)) => Ok(Float(left - right)),
        _ => panic!(),
    }
}

fn lt<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => Ok(Bool(left < right)),
        (Float(left), Float(right)) => Ok(Bool(left < right)),
        _ => panic!(),
    }
}

fn gt<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Int(left), Int(right)) => Ok(Bool(left > right)),
        (Float(left), Float(right)) => Ok(Bool(left > right)),
        _ => panic!(),
    }
}

fn leq<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    if let Bool(val) = gt(left, right, ast, state)? {
        Ok(Bool(!val))
    } else {
        panic!();
    }
}

fn geq<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    if let Bool(val) = lt(left, right, ast, state)? {
        Ok(Bool(!val))
    } else {
        panic!();
    }
}

fn eq<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        (Bool(left), Bool(right)) => Ok(Bool(left == right)),
        (Int(left), Int(right)) => Ok(Bool(left == right)),
        (Float(left), Float(right)) => Ok(Bool(left == right)),
        _ => panic!(),
    }
}

fn neq<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    if let Bool(val) = eq(left, right, ast, state)? {
        Ok(Bool(!val))
    } else {
        panic!();
    }
}

fn and<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
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

fn or<'a, 'b>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
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

fn check_zero<'a, 'b>(val: Literal<'a>, span: Span<'a>) -> Result<Literal<'a>, IntprtError<'a>> {
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
