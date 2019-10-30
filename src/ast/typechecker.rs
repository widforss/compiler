mod error;
mod op;
mod state;
mod util;

use super::{
    Ast, BinOp, Error, Expr, Func, Literal, Mutability, Span, State, Statement, Stmt, Type, UnOp,
    Value,
};
use error::{ErrorKind, TypeError};
use state::TypeVariable;

impl<'a> Ast<'a> {
    pub fn typecheck(&'a self) -> Result<(), TypeError<'a>> {
        let Ast(ast) = self;
        for function in ast.values() {
            check_fn(&function, self)?;
        }
        Ok(())
    }
}

fn check_fn<'a>(function: &'a Func<'a>, ast: &'a Ast<'a>) -> Result<(), TypeError<'a>> {
    let typestate = &mut State::new();
    let mut param_iter = function.params.iter();
    while let Some((typ, mutable, ident)) = param_iter.next() {
        let var_type = TypeVariable {
            typ: typ,
            mutable: *mutable,
        };
        typestate.insert(*ident, var_type);
    }

    let typ = check_stmt(&function.body, &function.typ, ast, typestate)?;
    if let Some(_) = typ {
        Ok(())
    } else {
        Err(TypeError::new(Some(function.span), ErrorKind::ReturnType))
    }
}

fn check_stmt<'a, 'b>(
    stmt: &'a Stmt<'a>,
    typ: &'a Type,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    use Statement::*;

    match &stmt.stmt {
        Let(mutability, ident, var_typ, expr) => {
            check_let(*mutability, *ident, var_typ, &expr, ast, typestate)
        }
        Assign(ident, expr) => check_assign(ident, &expr, ast, typestate),
        While(cond, body) => check_while(&cond, &**body, typ, ast, typestate),
        IfElse(cond, body, els) => check_ifelse(&cond, &**body, &**els, typ, ast, typestate),
        Block(vector) => check_block(vector, typ, ast, typestate),
        Return(expr) => check_return(&expr, typ, ast, typestate),
        Print(expr) => check_print(&expr, ast, typestate),
    }
}

fn check_let<'a, 'b>(
    mutable: Mutability,
    ident: &'a str,
    var_type: &'a Type,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    let actual_type = util::check_expr(expr, ast, typestate)?;
    util::check_type(&actual_type, var_type, expr.span)?;

    let var = TypeVariable {
        typ: var_type,
        mutable,
    };
    typestate.insert(ident, var);

    Ok(None)
}

fn check_assign<'a, 'b>(
    ident_expr: &'a Expr<'a>,
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    let typevar = deref_ident(ident_expr, typestate)?;
    let typ = util::check_expr(expr, ast, typestate)?;

    if !typevar.mutable {
        return Err(TypeError::new(Some(ident_expr.span), ErrorKind::NotMutable));
    }

    util::check_type(typevar.typ, &typ, expr.span)?;

    Ok(None)
}

fn check_while<'a, 'b>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    typ: &'a Type,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    let cond_type = util::check_expr(cond, ast, typestate)?;
    util::check_type(&cond_type, &Type::Bool, cond.span)?;

    if let Some(return_type) = check_stmt(body, typ, ast, typestate)? {
        util::check_type(&return_type, typ, body.span)?;
        return Ok(Some(return_type));
    }

    Ok(None)
}

fn check_ifelse<'a, 'b>(
    cond: &'a Expr<'a>,
    body: &'a Stmt<'a>,
    els: &'a Stmt<'a>,
    typ: &'a Type,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    let mut return_type = None;

    let cond_type = util::check_expr(cond, ast, typestate)?;
    util::check_type(&cond_type, &Type::Bool, cond.span)?;

    if let Some(body_type) = check_stmt(body, typ, ast, typestate)? {
        util::check_type(&body_type, typ, body.span)?;
        return_type = Some(body_type);
    }
    if let Some(els_type) = check_stmt(els, typ, ast, typestate)? {
        util::check_type(&els_type, typ, els.span)?;
        return_type = Some(els_type);
    }

    Ok(return_type)
}

fn check_block<'a, 'b>(
    block: &'a Vec<Stmt<'a>>,
    typ: &'a Type,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    let mut return_val = None;

    typestate.inc();
    for statement in block.iter() {
        if let Some(return_type) = check_stmt(statement, typ, ast, typestate)? {
            util::check_type(&return_type, typ, statement.span)?;
            return_val = Some(return_type);
        }
    }
    typestate.dec();

    Ok(return_val)
}

fn check_return<'a, 'b>(
    expr: &'a Expr<'a>,
    typ: &'a Type,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    let expr_type = util::check_expr(expr, ast, typestate)?;
    util::check_type(&expr_type, typ, expr.span)?;
    Ok(Some(expr_type))
}

fn check_print<'a, 'b>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<Option<Type>, TypeError<'a>> {
    util::check_expr(expr, ast, typestate)?;
    Ok(None)
}

fn deref_ident<'a, 'b>(
    ident: &'a Expr<'a>,
    typestate: &'b mut State<TypeVariable<'a>>,
) -> Result<TypeVariable<'a>, TypeError<'a>> {
    match &ident.value {
        Value::Ident(ident_string) => {
            let var = match typestate.get(ident_string) {
                Some(value) => value,
                None => return Err(TypeError::new(Some(ident.span), ErrorKind::VarNotFound)),
            };
            Ok(*var)
        }
        Value::UnOp(UnOp::Deref, ident) => {
            let typevar = deref_ident(&*ident, typestate)?;
            match typevar.typ {
                Type::Ref(true, typ) => {
                    let typevar = TypeVariable {
                        typ: typ,
                        mutable: true,
                    };
                    Ok(typevar)
                }
                Type::Ref(false, _) => {
                    Err(TypeError::new(Some(ident.span), ErrorKind::RefNotMutable))
                }
                _ => Err(TypeError::new(Some(ident.span), ErrorKind::DerefRef)),
            }
        }
        _ => panic!(),
    }
}
