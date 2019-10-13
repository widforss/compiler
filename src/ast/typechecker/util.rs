use super::{Ast, Expr, Literal, Span, Type, Value, State, TypeError, ErrorKind, op, TypeVariable};
use std::mem;

pub fn check_type<'a>(typ1: Type, typ2: Type, span: Span<'a>) -> Result<(), TypeError<'a>> {
    if mem::discriminant(&typ1) == mem::discriminant(&typ2) {
        Ok(())
    } else {
        Err(TypeError::new(Some(span), ErrorKind::TypeError))
    }
}

pub fn check_expr<'a>(
    expr: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    use Type::*;

    match &expr.value {
        Value::Literal(Literal::Unit) => Ok(Unit),
        Value::Literal(Literal::Bool(_)) => Ok(Bool),
        Value::Literal(Literal::Int(_)) => Ok(Int),
        Value::Literal(Literal::Float(_)) => Ok(Float),
        Value::BinOp(binop, left, right) => op::check_binop(*binop, left, right, expr.span, ast, typestate),
        Value::UnOp(unop, arg) => op::check_unop(*unop, arg, ast, typestate),
        Value::Call(ident, args) => check_call(&ident, &args, expr.span, ast, typestate),
        Value::Ident(ident) => {
            let typ = match typestate.get(*ident) {
                Some(value) => value.typ,
                None => return Err(TypeError::new(Some(expr.span), ErrorKind::VarNotFound)),
            };
            Ok(typ)
        }
    }
}

fn check_call<'a>(
    ident: &Span,
    args: &'a Vec<Expr<'a>>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let Ast(map) = ast;
    let func = match map.get(&ident.fragment[..]) {
        Some(func) => func,
        _ => return Err(TypeError::new(Some(span), ErrorKind::FuncNotFound)),
    };

    if func.params.len() != args.len() {
        return Err(TypeError::new(Some(span), ErrorKind::ArgsNum));
    };

    let arg_types = check_args(&args, ast, typestate)?;
    let mut param_iter = func.params.iter();
    for arg_type in arg_types.iter() {
        let (param_type, _, ident) = *param_iter.next().unwrap();
        check_type(param_type, *arg_type, ident)?;
    }

    Ok(func.typ)
}

fn check_args<'a>(
    args: &'a Vec<Expr<'a>>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Vec<(Type)>, TypeError<'a>> {
    let mut arg_types = vec![];
    for arg in args.iter() {
        let typ = check_expr(arg, ast, typestate)?;
        arg_types.push(typ);
    }
    Ok(arg_types)
}
