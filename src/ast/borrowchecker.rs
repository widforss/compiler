mod error;
mod util;

use super::{
    Ast, Error, Expr, Func, Lifetimes, Literal, Span, State, Statement, Stmt, UnOp, Value,
};
use error::{BorrowError, ErrorKind};

impl<'a> Ast<'a> {
    pub fn borrowcheck(&'a self) -> Result<(), BorrowError<'a>> {
        for (_, function) in self.sort_ast() {
            borrow_fn(&function, self)?;
        }
        Ok(())
    }
}

fn borrow_fn<'a>(function: &'a Func<'a>, ast: &'a Ast<'a>) -> Result<(), BorrowError<'a>> {
    let borrowstate = &mut State::new();
    let mut param_iter = function.params.iter();
    let mut var_id = 0;
    let Lifetimes(lifes) = &function.life;
    while let Some(param) = param_iter.next() {
        let Lifetimes(param_lifetimes) = param.lifetimes.clone();

        for life in param_lifetimes.iter() {
            if let None = function.lifetimes.get(life) {
                return Err(BorrowError::new(
                    Some(param.span),
                    ErrorKind::UndeclaredLifetime,
                ));
            }
        }

        borrowstate.insert(param.ident, (param_lifetimes, var_id));
        var_id += 1;
    }

    for life in lifes.iter() {
        if let None = function.lifetimes.get(life) {
            return Err(BorrowError::new(
                Some(function.return_span),
                ErrorKind::UndeclaredLifetime,
            ));
        }
    }

    borrow_stmt(&function.body, lifes, &mut var_id, ast, borrowstate)
}

fn borrow_stmt<'a, 'b>(
    stmt: &'a Stmt<'a>,
    life: &'a Vec<&'a str>,
    var_id: &'b mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    use Statement::*;

    match &stmt.stmt {
        Let(_, ident, _, expr) => borrow_let(*ident, &expr, var_id, ast, borrowstate),
        Assign(ident, expr) => borrow_assign(ident, &expr, var_id, ast, borrowstate),
        While(cond, body) => borrow_while(&cond, &**body, life, var_id, ast, borrowstate),
        IfElse(cond, body, els) => {
            borrow_ifelse(&cond, &**body, &**els, life, var_id, ast, borrowstate)
        }
        Block(vector) => borrow_block(vector, life, var_id, ast, borrowstate),
        Return(expr) => borrow_return(&expr, life, ast, borrowstate),
        Print(expr) => borrow_print(&expr, ast, borrowstate),
    }
}

fn borrow_let<'a, 'b>(
    ident: &'a str,
    expr: &'a Expr<'a>,
    var_id: &'b mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    let (life, id) = util::borrow_expr(expr, var_id, ast, borrowstate)?;
    borrowstate.insert(ident, (life, id));
    Ok(())
}

fn borrow_assign<'a, 'b>(
    ident_expr: &'a Expr<'a>,
    expr: &'a Expr<'a>,
    var_id: &'b mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    fn extract_ident<'a>(expr: &'a Expr<'a>) -> (usize, &'a str) {
        match &expr.value {
            Value::Ident(ident) => (0, ident),
            Value::UnOp(UnOp::Deref, arg) => {
                let (dereferences, ident) = extract_ident(arg);
                (dereferences + 1, ident)
            }
            _ => panic!(),
        }
    }

    let (rhs_life, rhs_id) = util::borrow_expr(expr, var_id, ast, borrowstate)?;

    let (dereferences, ident) = extract_ident(ident_expr);
    let (statelife, state_id) = borrowstate.get_mut(ident).unwrap();
    let mut dereflife = statelife
        .get(statelife.len() - dereferences..)
        .unwrap()
        .to_vec();

    *statelife = rhs_life;
    statelife.append(&mut dereflife);
    *state_id = rhs_id;
    Ok(())
}

fn borrow_while<'a, 'b>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    life: &'a Vec<&'a str>,
    var_id: &'b mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    util::borrow_expr(cond, &mut 0, ast, borrowstate)?;
    borrow_stmt(body, life, var_id, ast, borrowstate)?;
    Ok(())
}

fn borrow_ifelse<'a, 'b>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    els: &'a Stmt<'a>,
    life: &'a Vec<&'a str>,
    var_id: &'b mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    util::borrow_expr(cond, &mut 0, ast, borrowstate)?;
    borrow_stmt(body, life, var_id, ast, borrowstate)?;
    borrow_stmt(els, life, var_id, ast, borrowstate)?;
    Ok(())
}

fn borrow_block<'a, 'b>(
    block: &'a Vec<Stmt<'a>>,
    life: &'a Vec<&'a str>,
    var_id: &'b mut u64,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    borrowstate.inc();
    for statement in block.iter() {
        borrow_stmt(statement, life, var_id, ast, borrowstate)?;
    }
    borrowstate.dec();

    Ok(())
}

fn borrow_return<'a, 'b>(
    expr: &'a Expr<'a>,
    life: &'a Vec<&'a str>,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    let (lifetimes, _) = util::borrow_expr(expr, &mut 0, ast, borrowstate)?;

    if *life == lifetimes {
        Ok(())
    } else {
        Err(BorrowError::new(
            Some(expr.span),
            ErrorKind::ReturnedLifetime,
        ))
    }
}

fn borrow_print<'a, 'b>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    borrowstate: &'b mut State<(Vec<&'a str>, u64)>,
) -> Result<(), BorrowError<'a>> {
    util::borrow_expr(expr, &mut 0, ast, borrowstate)?;
    Ok(())
}
