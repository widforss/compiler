use super::super::{Ast, BinOp, Expr, Literal, Span, UnOp};
use super::error::{Error, ErrorKind};
use super::state::State;
use super::util;
use super::LiteralSpan;

use Literal::*;

pub fn intprt_binop<'a>(
    call: BinOp,
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<LiteralSpan<'a>, Error<'a>> {
    use BinOp::*;

    let literal = match call {
        Pow => pow_(left, right, span, ast, state),
        Mult => mult(left, right, span, ast, state),
        Rem => rem(left, right, span, ast, state),
        IntDiv => int_div(left, right, span, ast, state),
        Div => div(left, right, span, ast, state),
        Add => add(left, right, span, ast, state),
        Sub => sub(left, right, span, ast, state),
        Lt => lt(left, right, span, ast, state),
        Gt => gt(left, right, span, ast, state),
        Leq => leq(left, right, span, ast, state),
        Geq => geq(left, right, span, ast, state),
        Eq => eq(left, right, span, ast, state),
        Neq => neq(left, right, span, ast, state),
        And => and(left, right, span, ast, state),
        Or => or(left, right, span, ast, state),
    };
    Ok((literal?, span))
}

pub fn intprt_unop<'a>(
    call: UnOp,
    arg: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<LiteralSpan<'a>, Error<'a>> {
    use UnOp::*;

    let literal = match call {
        Sub => un_sub(arg, ast, state),
        Inv => un_inv(arg, ast, state),
    };
    Ok((literal?, span))
}

fn un_sub<'a>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        (Int(val), _) => Ok(Int(-val)),
        (Float(val), _) => Ok(Float(-val)),
        (_, span) => throw_type(span),
    }
}

fn un_inv<'a>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let arg = util::intprt_expr(arg, ast, state)?;
    match arg {
        (Bool(val), _) => Ok(Bool(!val)),
        (_, span) => throw_type(span),
    }
}

fn pow_<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => {
            let val = check(pow_ii(left, right), span)?;
            Ok(Int(val))
        }
        ((Float(left), _), (Float(right), _)) => Ok(Float(left.powf(right))),
        ((Float(left), _), (Int(right), _)) => Ok(Float(left.powf(right as f64))),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn mult<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => {
            let val = check(left.checked_mul(right), span)?;
            Ok(Int(val))
        }
        ((Float(left), _), (Float(right), _)) => Ok(Float(left * right)),
        ((Int(left), _), (Float(right), _)) => Ok(Float(left as f64 * right)),
        ((Float(left), _), (Int(right), _)) => Ok(Float(left * right as f64)),
        ((_, span), (Int(_), _)) => throw_type(span),
        ((Int(_), _), (_, span)) => throw_type(span),
        ((_, span), (Float(_), _)) => throw_type(span),
        ((Float(_), _), (_, span)) => throw_type(span),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn rem<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => {
            check_zero(Int(right), span)?;
            let val = check(left.checked_rem(right), span)?;
            Ok(Int(val))
        }
        ((Float(left), _), (Float(right), _)) => Ok(Float(left % right)),
        ((Int(left), _), (Float(right), _)) => Ok(Float(left as f64 % right)),
        ((Float(left), _), (Int(right), _)) => Ok(Float(left % right as f64)),
        ((_, span), (Int(_), _)) => throw_type(span),
        ((Int(_), _), (_, span)) => throw_type(span),
        ((_, span), (Float(_), _)) => throw_type(span),
        ((Float(_), _), (_, span)) => throw_type(span),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn int_div<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
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
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => {
            check_zero(Int(right), span)?;
            let val = check(left.checked_div(right), span)?;
            Ok(Int(val))
        }
        ((Float(left), _), (Float(right), _)) => Ok(Float(left / right)),
        ((Int(left), _), (Float(right), _)) => Ok(Float(left as f64 / right)),
        ((Float(left), _), (Int(right), _)) => Ok(Float(left / right as f64)),
        ((_, span), (Int(_), _)) => throw_type(span),
        ((Int(_), _), (_, span)) => throw_type(span),
        ((_, span), (Float(_), _)) => throw_type(span),
        ((Float(_), _), (_, span)) => throw_type(span),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn add<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => {
            let val = check(left.checked_add(right), span)?;
            Ok(Int(val))
        }
        ((Float(left), _), (Float(right), _)) => Ok(Float(left + right)),
        ((Int(left), _), (Float(right), _)) => Ok(Float(left as f64 + right)),
        ((Float(left), _), (Int(right), _)) => Ok(Float(left + right as f64)),
        ((_, span), (Int(_), _)) => throw_type(span),
        ((Int(_), _), (_, span)) => throw_type(span),
        ((_, span), (Float(_), _)) => throw_type(span),
        ((Float(_), _), (_, span)) => throw_type(span),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn sub<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => {
            let val = check(left.checked_sub(right), span)?;
            Ok(Int(val))
        }
        ((Float(left), _), (Float(right), _)) => Ok(Float(left - right)),
        ((Int(left), _), (Float(right), _)) => Ok(Float(left as f64 - right)),
        ((Float(left), _), (Int(right), _)) => Ok(Float(left - right as f64)),
        ((_, span), (Int(_), _)) => throw_type(span),
        ((Int(_), _), (_, span)) => throw_type(span),
        ((_, span), (Float(_), _)) => throw_type(span),
        ((Float(_), _), (_, span)) => throw_type(span),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn lt<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => Ok(Bool(left < right)),
        ((Float(left), _), (Float(right), _)) => Ok(Bool(left < right)),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn gt<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => Ok(Bool(left > right)),
        ((Float(left), _), (Float(right), _)) => Ok(Bool(left > right)),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn leq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => Ok(Bool(left <= right)),
        ((Float(left), _), (Float(right), _)) => Ok(Bool(left <= right)),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn geq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Int(left), _), (Int(right), _)) => Ok(Bool(left >= right)),
        ((Float(left), _), (Float(right), _)) => Ok(Bool(left >= right)),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn eq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Bool(left), _), (Bool(right), _)) => Ok(Bool(left == right)),
        ((Int(left), _), (Int(right), _)) => Ok(Bool(left == right)),
        ((Float(left), _), (Float(right), _)) => Ok(Bool(left == right)),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn neq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    let left = util::intprt_expr(left, ast, state)?;
    let right = util::intprt_expr(right, ast, state)?;
    match (left, right) {
        ((Bool(left), _), (Bool(right), _)) => Ok(Bool(left != right)),
        ((Int(left), _), (Int(right), _)) => Ok(Bool(left != right)),
        ((Float(left), _), (Float(right), _)) => Ok(Bool(left != right)),
        ((_, _), (_, _)) => throw_type(span),
    }
}

fn and<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    match util::intprt_expr(left, ast, state)? {
        (Bool(false), _) => Ok(Bool(false)),
        (Bool(true), _) => {
            if let (Bool(bool), _) = util::intprt_expr(right, ast, state)? {
                Ok(Bool(bool))
            } else {
                throw_type(span)
            }
        }
        (_, _) => throw_type(span),
    }
}

fn or<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Literal, Error<'a>> {
    match util::intprt_expr(left, ast, state)? {
        (Bool(true), _) => Ok(Bool(true)),
        (Bool(false), _) => {
            if let (Bool(bool), _) = util::intprt_expr(right, ast, state)? {
                Ok(Bool(bool))
            } else {
                throw_type(span)
            }
        }
        (_, _) => throw_type(span),
    }
}

fn throw_type(span: Span) -> Result<Literal, Error> {
    Err(Error::new(Some(span), ErrorKind::TypeError))
}

fn check_zero<'a>(val: Literal, span: Span<'a>) -> Result<Literal, Error<'a>> {
    match val {
        Int(0) => Err(Error::new(Some(span), ErrorKind::DivisionByZero)),
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

fn check<'a, T>(val: Option<T>, span: Span<'a>) -> Result<T, Error> {
    match val {
        Some(val) => Ok(val),
        None => Err(Error::new(Some(span), ErrorKind::Overflow)),
    }
}
