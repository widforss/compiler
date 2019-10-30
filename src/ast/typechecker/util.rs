use super::{op, Ast, ErrorKind, Expr, Literal, Span, State, Type, TypeError, TypeVariable, Value};
use std::mem;

pub fn check_type<'a, 'b>(
    typ1: &'a Type,
    typ2: &'a Type,
    span: Span<'b>,
) -> Result<(), TypeError<'b>> {
    if let (Type::Ref(mut1, typ1), Type::Ref(mut2, typ2)) = (typ1, typ2) {
        if mut1 == mut2 {
            return check_type(typ1, typ2, span);
        }
    } else if mem::discriminant(typ1) == mem::discriminant(typ2) {
        return Ok(());
    }
    Err(TypeError::new(Some(span), ErrorKind::TypeError))
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
        Value::Literal(Literal::Ref(pointer)) => {
            let val = typestate.deref(*pointer);
            Ok(Ref(val.mutable, Box::new(val.typ.clone())))
        }
        Value::BinOp(binop, left, right) => {
            op::check_binop(*binop, left, right, expr.span, ast, typestate)
        }
        Value::UnOp(unop, arg) => op::check_unop(*unop, arg, ast, typestate),
        Value::Call(ident, args) => check_call(&ident, &args, expr.span, ast, typestate),
        Value::Ident(ident) => {
            let typ = match typestate.get(*ident) {
                Some(value) => value.typ,
                None => return Err(TypeError::new(Some(expr.span), ErrorKind::VarNotFound)),
            };
            Ok(typ.clone())
        }
    }
}

fn check_call<'a>(
    ident: &'a str,
    args: &'a Vec<Expr<'a>>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let Ast(map) = ast;
    let func = match map.get(ident) {
        Some(func) => func,
        _ => return Err(TypeError::new(Some(span), ErrorKind::FuncNotFound)),
    };

    if func.params.len() != args.len() {
        return Err(TypeError::new(Some(span), ErrorKind::ArgsNum));
    };

    let mut param_iter = func.params.iter();
    for arg in args.iter() {
        let typ = check_expr(arg, ast, typestate)?;
        let (param_type, _, _) = param_iter.next().unwrap();
        check_type(&param_type, &typ, arg.span)?;
    }

    Ok(func.typ.clone())
}
