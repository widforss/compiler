use super::{Ast, BinOp, Expr, Type, Span, UnOp, State, TypeError, ErrorKind, util, TypeVariable};

use Type::*;

pub fn check_binop<'a>(
    call: BinOp,
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    use BinOp::*;

    let literal = match call {
        Pow => pow_(left, right, span, ast, typestate),
        Mult | Rem | IntDiv | Div | Add | Sub => arith(left, right, span, ast, typestate),
        Lt | Gt | Leq | Geq => cmp(left, right, span, ast, typestate),
        Eq | Neq => eq(left, right, span, ast, typestate),
        And | Or => logic(left, right, span, ast, typestate),
    };
    Ok(literal?)
}

pub fn check_unop<'a>(
    call: UnOp,
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    use UnOp::*;

    let literal = match call {
        Sub => un_sub(arg, ast, typestate),
        Inv => un_inv(arg, ast, typestate),
    };
    Ok(literal?)
}

fn un_sub<'a>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let arg_type = util::check_expr(arg, ast, typestate)?;
    match arg_type {
        Int => Ok(Int),
        Float => Ok(Float),
        _ => throw_type(arg.span),
    }
}

fn un_inv<'a>(
    arg: &'a Expr<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let arg_type = util::check_expr(arg, ast, typestate)?;
    match arg_type {
        Bool => Ok(Bool),
        _ => throw_type(arg.span),
    }
}

fn pow_<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let left_type = util::check_expr(left, ast, typestate)?;
    let right_type = util::check_expr(right, ast, typestate)?;
    match (left_type, right_type) {
        (Int, Int) => Ok(Int),
        (Float, Float) => Ok(Float),
        (Float, Int) => Ok(Float),
        _ => throw_type(span),
    }
}

fn arith<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let left_type = util::check_expr(left, ast, typestate)?;
    let right_type = util::check_expr(right, ast, typestate)?;
    match (left_type, right_type) {
        (Int, Int) => Ok(Int),
        (Float, Float) => Ok(Float),
        (Int, Float) => Ok(Float),
        (Float, Int) => Ok(Float),
        (_, Int) => throw_type(left.span),
        (Int, _) => throw_type(right.span),
        (_, Float) => throw_type(left.span),
        (Float, _) => throw_type(right.span),
        _ => throw_type(span),
    }
}

fn cmp<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let left_type = util::check_expr(left, ast, typestate)?;
    let right_type = util::check_expr(right, ast, typestate)?;
    match (left_type, right_type) {
        (Int, Int) => Ok(Bool),
        (Float, Float) => Ok(Bool),
        (_, Int) => throw_type(left.span),
        (Int, _) => throw_type(right.span),
        (_, Float) => throw_type(left.span),
        (Float, _) => throw_type(right.span),
        _ => throw_type(span),
    }
}

fn eq<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let left_type = util::check_expr(left, ast, typestate)?;
    let right_type = util::check_expr(right, ast, typestate)?;
    match (left_type, right_type) {
        (Bool, Bool) => Ok(Bool),
        (Int, Int) => Ok(Bool),
        (Float, Float) => Ok(Bool),
        (_, Bool) => throw_type(left.span),
        (Bool, _) => throw_type(right.span),
        (_, Int) => throw_type(left.span),
        (Int, _) => throw_type(right.span),
        (_, Float) => throw_type(left.span),
        (Float, _) => throw_type(right.span),
        _ => throw_type(span),
    }
}

fn logic<'a>(
    left: &'a Expr<'a>,
    right: &'a Expr<'a>,
    span: Span<'a>,
    ast: &'a Ast<'a>,
    typestate: &mut State<TypeVariable>,
) -> Result<Type, TypeError<'a>> {
    let left_type = util::check_expr(left, ast, typestate)?;
    let right_type = util::check_expr(right, ast, typestate)?;
    match (left_type, right_type) {
        (Bool, Bool) => Ok(Bool),
        (_, Bool) => throw_type(left.span),
        (Bool, _) => throw_type(right.span),
        _ => throw_type(span),
    }
}

fn throw_type(span: Span) -> Result<Type, TypeError> {
    Err(TypeError::new(Some(span), ErrorKind::TypeError))
}
