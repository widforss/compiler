use super::{op, State, IntprtError, Ast, Expr, Literal, Value};

pub fn intprt_expr<'a>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    use Value::*;

    match &expr.value {
        Literal(val) => Ok(*val),
        BinOp(binop, left, right) => op::intprt_binop(*binop, left, right, expr.span, ast, state),
        UnOp(unop, arg) => op::intprt_unop(*unop, arg, ast, state),
        Call(ident, args) => super::intprt_call(&ident, &args, ast, state),
        Ident(ident) => Ok(*state.get(*ident).unwrap()),
    }
}
