mod error;
mod op;
mod util;

use super::{Ast, Expr, Literal, Span, Statement, Stmt, Value, BinOp, UnOp, State, Error};
use error::{IntprtError, ErrorKind};

const STACK_DEPTH: u64 = 1000;

impl<'a> Ast<'a> {
    pub fn run(&'a self, main_call: &'a Expr<'a>) -> Result<(), IntprtError<'a>> {
        match util::intprt_expr(main_call, self, &mut State::new(1)) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }
}

fn intprt_call<'a>(
    ident: &Span,
    args: &'a Vec<Expr<'a>>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Literal, IntprtError<'a>> {
    let Ast(map) = ast;
    let func = map.get(&ident.fragment[..]).unwrap();

    if state.depth() > STACK_DEPTH {
        return Err(IntprtError::new(Some(func.span), ErrorKind::StackDepth));
    }

    let args = intprt_args(&args, ast, state)?;
    let state = &mut State::new(state.depth() + 1);
    let mut param_iter = func.params.iter();
    for value in args.iter() {
        let (_, _, ident) = *param_iter.next().unwrap();
        state.insert(ident, *value);
    }

    let return_val = intprt_stmt(&func.body, ast, state)?;
    Ok(return_val.unwrap())
}

fn intprt_stmt<'a>(
    stmt: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
    use Statement::*;

    match &stmt.stmt {
        Let(_, ident, _, expr) => intprt_let(*ident, &expr, ast, state),
        Assign(ident, expr) => intprt_assign(*ident, &expr, ast, state),
        While(cond, body) => intprt_while(&cond, &**body, ast, state),
        IfElse(cond, body, els) => intprt_ifelse(&cond, &**body, &**els, ast, state),
        Block(vector) => intprt_block(vector, ast, state),
        Return(expr) => intprt_return(&expr, ast, state),
        Print(expr) => intprt_print(&expr, ast, state),
    }
}

fn intprt_let<'a>(
    ident: Span<'a>,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
    let value = util::intprt_expr(expr, ast, state)?;
    state.insert(ident, value);
    Ok(None)
}

fn intprt_assign<'a>(
    ident: Span<'a>,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
    let value = util::intprt_expr(expr, ast, state)?;
    state.set(ident, value);
    Ok(None)
}

fn intprt_while<'a>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
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

fn intprt_ifelse<'a>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    els: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
    let cond = util::intprt_expr(cond, ast, state)?;
    if let Literal::Bool(true) = cond {
        intprt_stmt(body, ast, state)
    } else {
        intprt_stmt(els, ast, state)
    }
}

fn intprt_block<'a>(
    block: &'a Vec<Stmt<'a>>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
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

fn intprt_return<'a>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
    let value = util::intprt_expr(expr, ast, state)?;
    Ok(Some(value))
}

fn intprt_print<'a>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Option<Literal>, IntprtError<'a>> {
    use Literal::*;

    let string = match util::intprt_expr(expr, ast, state)? {
        Unit => format!("()"),
        Bool(val) => format!("{}", val),
        Int(val) => format!("{}", val),
        Float(val) => format!("{}", val),
    };
    println!("{}", string);
    Ok(None)
}

fn intprt_args<'a>(
    args: &'a Vec<Expr<'a>>,
    ast: &'a Ast<'a>,
    state: &mut State<Literal>,
) -> Result<Vec<Literal>, IntprtError<'a>> {
    let mut interpreted_args = vec![];
    for arg in args.iter() {
        let literal = util::intprt_expr(arg, ast, state)?;
        interpreted_args.push(literal);
    }
    Ok(interpreted_args)
}
