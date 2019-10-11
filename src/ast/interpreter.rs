mod error;
mod op;
mod state;
mod util;

use super::{Ast, Expr, Literal, Mutability, Span, Statement, Stmt, Type};
pub use error::Error;
use error::ErrorKind;
use state::{State, Variable};

pub type LiteralSpan<'a> = (Literal, Span<'a>);

impl<'a> Ast<'a> {
    pub fn run(&'a self, main_call: &'a Expr<'a>) -> Result<(), Error<'a>> {
        match util::intprt_expr(main_call, self, &mut State::new()) {
            Ok((_, _)) => Ok(()),
            Err(err) => Err(err),
        }
    }
}

pub fn intprt_call<'a>(
    ident: &Span,
    args: &'a Vec<Expr<'a>>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<LiteralSpan<'a>, Error<'a>> {
    let Ast(map) = ast;
    let func = match map.get(&ident.fragment[..]) {
        Some(func) => func,
        _ => return Err(Error::new(Some(span), ErrorKind::FuncNotFound)),
    };

    if func.params.len() != args.len() {
        return Err(Error::new(Some(span), ErrorKind::ArgsNum));
    };

    let args = intprt_args(&args, ast, state)?;
    let state = &mut State::new();
    let mut param_iter = func.params.iter();
    for (value, argspan) in args.iter() {
        let (typ, mutable, ident) = *param_iter.next().unwrap();
        let value = util::check_type(typ, *value, *argspan)?;
        let var = Variable {
            typ,
            mutable,
            value,
        };
        state.insert(ident, var);
    }

    if let Some((literal, span)) = intprt_stmt(&func.body, ast, state)? {
        match util::check_type(func.typ, literal, span) {
            Ok(res) => Ok((res, span)),
            Err(_) => Err(Error::new(Some(span), ErrorKind::ReturnType)),
        }
    } else {
        return Err(Error::new(Some(func.span), ErrorKind::ReturnType));
    }
}

fn intprt_stmt<'a>(
    stmt: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    use Statement::*;

    match &stmt.stmt {
        Let(mutability, ident, typ, expr) => {
            intprt_let(*mutability, *ident, *typ, &expr, ast, state)
        }
        Assign(ident, expr) => intprt_assign(*ident, &expr, ast, state),
        While(cond, body) => intprt_while(&cond, &**body, ast, state),
        IfElse(cond, body, els) => intprt_ifelse(&cond, &**body, &**els, ast, state),
        Block(vector) => intprt_block(vector, ast, state),
        Return(expr) => intprt_return(&expr, ast, state),
        Print(expr) => intprt_print(&expr, ast, state),
    }
}

fn intprt_let<'a>(
    mutable: Mutability,
    ident: Span<'a>,
    typ: Type,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    let (value, _) = util::intprt_expr(expr, ast, state)?;
    let value = util::check_type(typ, value, expr.span)?;
    let var = Variable {
        typ,
        mutable,
        value,
    };
    state.insert(ident, var);
    Ok(None)
}

fn intprt_assign<'a>(
    ident: Span<'a>,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    let (val, _) = util::intprt_expr(expr, ast, state)?;
    state.set(ident, val)?;
    Ok(None)
}

fn intprt_while<'a>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    loop {
        let (val, _) = util::intprt_expr(cond, ast, state)?;
        let cond = util::check_type(Type::Bool, val, cond.span)?;
        if let Literal::Bool(false) = cond {
            return Ok(None);
        }
        match intprt_stmt(body, ast, state)? {
            Some((result, span)) => return Ok(Some((result, span))),
            None => continue,
        };
    }
}

fn intprt_ifelse<'a>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    els: &'a Stmt<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    let (val, _) = util::intprt_expr(cond, ast, state)?;
    let cond = util::check_type(Type::Bool, val, cond.span)?;
    if let Literal::Bool(true) = cond {
        intprt_stmt(body, ast, state)
    } else {
        intprt_stmt(els, ast, state)
    }
}

fn intprt_block<'a>(
    block: &'a Vec<Stmt<'a>>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
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
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    let (lit, span) = util::intprt_expr(expr, ast, state)?;
    Ok(Some((lit, span)))
}

fn intprt_print<'a>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Option<LiteralSpan<'a>>, Error<'a>> {
    use Literal::*;

    let string = match util::intprt_expr(expr, ast, state)? {
        (Unit, _) => format!("()"),
        (Bool(val), _) => format!("{}", val),
        (Int(val), _) => format!("{}", val),
        (Float(val), _) => format!("{}", val),
    };
    println!("{}", string);
    Ok(None)
}

fn intprt_args<'a>(
    args: &'a Vec<Expr<'a>>,
    ast: &'a Ast<'a>,
    state: &mut State,
) -> Result<Vec<LiteralSpan<'a>>, Error<'a>> {
    let mut interpreted_args = vec![];
    for arg in args.iter() {
        let literal = util::intprt_expr(arg, ast, state)?;
        interpreted_args.push(literal);
    }
    Ok(interpreted_args)
}
