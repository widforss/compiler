use super::{op, Ast, Expr, IntprtError, Literal, State, Value};

pub fn intprt_expr<'a, 'b>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    use Value::*;

    match &expr.value {
        Literal(val) => Ok(*val),
        BinOp(binop, left, right) => op::intprt_binop(*binop, left, right, expr.span, ast, state),
        UnOp(unop, arg) => op::intprt_unop(*unop, arg, ast, state),
        Call(ident, args) => super::intprt_call(&ident, &args, ast, state),
        Ident(ident) => Ok(*state.get(*ident).unwrap()),
    }
}
