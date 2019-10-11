use super::super::{Ast, Expr, Literal, Span, Type, Value};
use super::error::{Error, ErrorKind};
use super::state::State;
use super::{op, LiteralSpan};

pub fn check_type<'a>(typ: Type, val: Literal, span: Span<'a>) -> Result<Literal, Error<'a>> {
    match (typ, val) {
        (Type::Unit, Literal::Unit) => Ok(val),
        (Type::Bool, Literal::Bool(_)) => Ok(val),
        (Type::Int, Literal::Int(_)) => Ok(val),
        (Type::Float, Literal::Float(_)) => Ok(val),
        _ => Err(Error::new(Some(span), ErrorKind::TypeError)),
    }
}

pub fn intprt_expr<'a>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<LiteralSpan<'a>, Error<'a>> {
    use Value::*;

    match &expr.value {
        Literal(val) => Ok((*val, expr.span)),
        BinOp(binop, left, right) => op::intprt_binop(*binop, left, right, expr.span, ast, state),
        UnOp(unop, arg) => op::intprt_unop(*unop, arg, expr.span, ast, state),
        Call(ident, args) => super::intprt_call(&ident, &args, expr.span, ast, state),
        Ident(ident) => {
            let variable = state.get(*ident)?;
            Ok((variable.value, expr.span))
        }
    }
}
