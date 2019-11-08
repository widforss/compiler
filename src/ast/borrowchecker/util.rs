use super::{Ast, BorrowError, ErrorKind, Expr, Lifetimes, Literal, State, UnOp, Value};
use std::collections::HashMap;
use std::collections::HashSet;

const LOCAL_LIFE: &'static str = "'!local";

pub fn borrow_expr<'a>(
    expr: &'a Expr<'a>,
    var_id: &mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &mut State<(Vec<&'a str>, u64)>,
) -> Result<(Vec<&'a str>, u64), BorrowError<'a>> {
    match &expr.value {
        Value::Literal(Literal::Ref(_)) => panic!(),

        Value::UnOp(UnOp::Ref(_), arg) => {
            let (mut inner_life, id) = borrow_expr(arg, var_id, ast, borrowstate)?;
            inner_life.push(LOCAL_LIFE);
            Ok((inner_life, id))
        }
        Value::UnOp(UnOp::Deref, arg) => {
            let (mut inner_life, id) = borrow_expr(arg, var_id, ast, borrowstate)?;
            inner_life.pop();
            Ok((inner_life, id))
        }

        Value::Literal(_) | Value::UnOp(_, _) | Value::BinOp(_, _, _) => {
            *var_id += 1;
            Ok((vec![], *var_id - 1))
        }

        Value::Call(ident, args) => borrow_call(&ident, &args, var_id, ast, borrowstate),
        Value::Ident(ident) => {
            let lifetimes = borrowstate.get(*ident).unwrap();
            Ok(lifetimes.clone())
        }
    }
}

fn borrow_call<'a>(
    ident: &'a str,
    args: &'a Vec<Expr<'a>>,
    var_id: &mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &mut State<(Vec<&'a str>, u64)>,
) -> Result<(Vec<&'a str>, u64), BorrowError<'a>> {
    let Ast(functions) = ast;
    let func = functions.get(ident).unwrap();

    let mut varset = HashSet::new();
    let mut lifemap: HashMap<&'a str, &'a str> = HashMap::new();
    let mut param_iter = func.params.iter();
    for arg in args.iter() {
        let (arg_lifes, id) = borrow_expr(arg, var_id, ast, borrowstate)?;
        let param = param_iter.next().unwrap();

        if varset.insert(id) == false {
            return Err(BorrowError::new(
                Some(arg.span),
                ErrorKind::SameReference
            ))
        }

        let Lifetimes(param_lifes) = &param.lifetimes;
        let mut param_life_iter = param_lifes.iter();
        for arg_life in arg_lifes.iter() {
            let param_life = *param_life_iter.next().unwrap();

            if let None = func.lifetimes.get(param_life) {
                return Err(BorrowError::new(
                    Some(param.span),
                    ErrorKind::UndeclaredLifetime,
                ));
            }

            if let Some(expect) = lifemap.insert(param_life, arg_life.clone()) {
                if &expect != arg_life {
                    return Err(BorrowError::new(Some(arg.span), ErrorKind::LifetimeError));
                }
            }
        }
    }

    let Lifetimes(func_lifes) = &func.life;
    for life in func_lifes.iter() {
        if let None = func.lifetimes.get(life) {
            return Err(BorrowError::new(
                Some(func.return_span),
                ErrorKind::UndeclaredLifetime,
            ));
        }
    }

    let mut lifetimes = vec![];
    let Lifetimes(func_lifes) = &func.life;
    for func_life in func_lifes {
        if let Some(translated) = lifemap.get(func_life) {
            lifetimes.push(*translated);
        } else {
            return Err(BorrowError::new(
                Some(func.return_span),
                ErrorKind::UnmappedLifetime,
            ))
        }
    }

    *var_id += 1;
    Ok((lifetimes, *var_id - 1))
}
