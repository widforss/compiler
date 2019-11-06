mod error;
mod op;
mod util;

use super::{Ast, BinOp, Error, Expr, Literal, Param, Span, State, Statement, Stmt, UnOp, Value};
use error::{ErrorKind, IntprtError};

const STACK_DEPTH: usize = 1000;

impl<'a> Ast<'a> {
    pub fn run(&'a self, main_call: &'a Expr<'a>) -> Result<(), IntprtError<'a>> {
        match util::intprt_expr(main_call, self, &mut State::new()) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }
}

fn intprt_call<'a, 'b>(
    ident: &'a str,
    args: &'a Vec<Expr<'a>>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Literal<'a>, IntprtError<'a>> {
    let Ast(map) = ast;
    let func = map.get(ident).unwrap();

    if state.depth() > STACK_DEPTH {
        return Err(IntprtError::new(Some(func.span), ErrorKind::StackDepth));
    }

    let args = intprt_args(&args, ast, state)?;
    state.add();
    let mut param_iter = func.params.iter();
    for value in args.iter() {
        let Param { ident, .. } = *param_iter.next().unwrap();
        state.insert(ident, *value);
    }

    let return_val = intprt_stmt(&func.body, ast, state)?;
    state.rem();
    Ok(return_val.unwrap())
}

fn intprt_stmt<'a, 'b>(
    stmt: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    use Statement::*;

    match &stmt.stmt {
        Let(_, ident, _, expr) => intprt_let(*ident, &expr, ast, state),
        Assign(ident, expr) => intprt_assign(ident, &expr, ast, state),
        While(cond, body) => intprt_while(&cond, &**body, ast, state),
        IfElse(cond, body, els) => intprt_ifelse(&cond, &**body, &**els, ast, state),
        Block(vector) => intprt_block(vector, ast, state),
        Return(expr) => intprt_return(&expr, ast, state),
        Print(expr) => intprt_print(&expr, ast, state),
    }
}

fn intprt_let<'a, 'b>(
    ident: &str,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    let value = util::intprt_expr(expr, ast, state)?;
    state.insert(ident, value);
    Ok(None)
}

fn intprt_assign<'a, 'b>(
    ident: &'a Expr<'a>,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    let value = util::intprt_expr(expr, ast, state)?;
    let var = intprt_assign_lhs(ident, state)?;
    *var = value;
    Ok(None)
}

fn intprt_while<'a, 'b>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    loop {
        let cond = util::intprt_expr(cond, ast, state)?;
        if let Literal::Bool(false) = cond {
            return Ok(None);
        }
        match intprt_stmt(body, ast, state)? {
            Some(result) => return Ok(Some(result)),
            None => continue,
        };
    }
}

fn intprt_ifelse<'a, 'b>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    els: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    let cond = util::intprt_expr(cond, ast, state)?;
    if let Literal::Bool(true) = cond {
        intprt_stmt(body, ast, state)
    } else {
        intprt_stmt(els, ast, state)
    }
}

fn intprt_block<'a, 'b>(
    block: &'a Vec<Stmt<'a>>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    let mut return_val = None;

    state.inc();
    for statement in block.iter() {
        if let Some(result) = intprt_stmt(statement, ast, state)? {
            return_val = Some(result);
            break;
        }
    }
    state.dec();
    Ok(return_val)
}

fn intprt_return<'a, 'b>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    let value = util::intprt_expr(expr, ast, state)?;
    Ok(Some(value))
}

fn intprt_print<'a, 'b>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Option<Literal<'a>>, IntprtError<'a>> {
    use Literal::*;
    let literal = util::intprt_expr(expr, ast, state)?;

    fn intprt<'a, 'b>(lit: Literal, state: &'b mut State<Literal<'a>>) -> String {
        match lit {
            Unit => format!("()"),
            Bool(val) => format!("{}", val),
            Int(val) => format!("{}", val),
            Float(val) => format!("{}", val),
            Ref(val) => format!("&{}", intprt(*state.deref(val), state)),
        }
    }

    let string = intprt(literal, state);
    println!("{}", string);
    Ok(None)
}

fn intprt_args<'a, 'b>(
    args: &'a Vec<Expr<'a>>,
    ast: &'a Ast<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<Vec<Literal<'a>>, IntprtError<'a>> {
    let mut interpreted_args = vec![];
    for arg in args.iter() {
        let literal = util::intprt_expr(arg, ast, state)?;
        interpreted_args.push(literal);
    }
    Ok(interpreted_args)
}

fn intprt_assign_lhs<'a, 'b>(
    expr: &'a Expr<'a>,
    state: &'b mut State<Literal<'a>>,
) -> Result<&'b mut Literal<'a>, IntprtError<'a>> {
    match &expr.value {
        Value::Ident(ident) => Ok(state.get_mut(*ident).unwrap()),
        Value::UnOp(UnOp::Deref, arg) => {
            if let Literal::Ref(pointer) = intprt_assign_lhs(arg, state)? {
                let pointer = pointer.clone();
                Ok(state.deref(pointer))
            } else {
                panic!();
            }
        }
        _ => panic!(),
    }
}
