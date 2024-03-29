mod error;
mod expr;
mod stmt;
mod util;

use super::{
    error::Error, Ast, BinOp, Expr, Func, Lifetimes, Literal, Param, Span, Statement, Stmt, Type,
    UnOp, Value,
};
use error::{ErrorKind, IResult, ParseError};
use nom::{
    branch, bytes::complete as bytes, character::complete as character, multi, sequence, Err,
};
use std::collections::HashMap;
use std::collections::HashSet;

impl<'a> Ast<'a> {
    pub fn parse(input: &'a str) -> Result<Self, ParseError> {
        match parse_fn(Span::new(input)) {
            Ok((Span { fragment: "", .. }, tree)) => Ok(tree),
            Ok((input, _)) => Err(ParseError::new(
                input,
                Some(input),
                ErrorKind::NotRecognised,
            )),
            Err(Err::Failure(ParseError { input, span, error })) => {
                Err(ParseError::new(input, span, error))
            }
            _ => panic!(),
        }
    }
}

fn parse_fn(input: Span) -> IResult<Span, Ast> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let parser = multi::many1(sequence::tuple((
        bytes::tag(""),
        sequence::preceded(
            sequence::tuple((bytes::tag("fn "), character::multispace0)),
            util::parse_ident_span,
        ),
        sequence::preceded(
            character::multispace0,
            sequence::delimited(bytes::tag("<"), parse_lifes, bytes::tag(">")),
        ),
        sequence::preceded(
            character::multispace0,
            sequence::delimited(bytes::tag("("), parse_params, bytes::tag(")")),
        ),
        sequence::preceded(
            sequence::tuple((
                character::multispace0,
                bytes::tag("->"),
                character::multispace0,
            )),
            sequence::tuple((bytes::tag(""), Type::parse_life)),
        ),
        bytes::tag(""),
        Stmt::parse,
        character::multispace0,
    )));
    let (input, mut functions) = match util::short(parser(input)) {
        Ok(res) => res,
        Err(Err::Failure(ParseError {
            input,
            error: ErrorKind::Nom(_),
            ..
        })) => {
            return Err(Err::Failure(ParseError::new(
                input,
                Some(input),
                ErrorKind::ParseFunction,
            )))
        }
        Err(err) => return Err(err),
    };

    let mut funcmap = HashMap::new();
    functions.reverse();
    let mut order = 0;
    while let Some((
        mut span,
        ident,
        lifetimes,
        params,
        (mut return_span, (typ, life)),
        return_end,
        body,
        end,
    )) = functions.pop()
    {
        let begin = span.offset - orig_input.offset;
        let end = end.offset - orig_input.offset;
        span.fragment = &orig_input.fragment[begin..end];

        let return_begin = return_span.offset - orig_input.offset;
        let return_end = return_end.offset - orig_input.offset;
        return_span.fragment = &orig_input.fragment[return_begin..return_end];

        if !funcmap.contains_key(ident.fragment) {
            funcmap.insert(
                ident.fragment,
                Func {
                    typ,
                    life,
                    lifetimes: lifetimes
                        .iter()
                        .map(|string| *string)
                        .collect::<HashSet<&str>>(),
                    params,
                    body,
                    span,
                    return_span,
                    order,
                },
            );
        } else {
            return Err(Err::Failure(ParseError::new(
                orig_input,
                Some(span),
                ErrorKind::DoubleFunctionDecl,
            )));
        }
        order += 1;
    }

    let mainfunc = match funcmap.get("main") {
        Some(res) => res,
        None => {
            return Err(Err::Failure(ParseError::new(
                orig_input,
                None,
                ErrorKind::NoMain,
            )))
        }
    };
    match mainfunc.typ {
        Type::Unit => Ok((input, Ast(funcmap))),
        _ => Err(Err::Failure(ParseError::new(
            orig_input,
            Some(mainfunc.span),
            ErrorKind::MainReturns,
        ))),
    }
}

fn parse_lifes(input: Span) -> IResult<Span, Vec<&str>> {
    let (input, _) = character::multispace0(input)?;

    let parser = multi::separated_list(
        sequence::preceded(character::multispace0, bytes::tag(",")),
        util::parse_lifetime,
    );
    let (input, lifes) = parser(input)?;
    let lifes = lifes.iter().map(|span| span.fragment).collect();
    Ok((input, lifes))
}

fn parse_params(input: Span) -> IResult<Span, Vec<Param>> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let parser = multi::separated_list(
        sequence::preceded(character::multispace0, bytes::tag(",")),
        sequence::tuple((
            bytes::tag(""),
            sequence::preceded(
                character::multispace0,
                branch::alt((bytes::tag("mut "), bytes::tag(""))),
            ),
            util::parse_ident_span,
            sequence::preceded(
                sequence::tuple((character::multispace0, bytes::tag(":"))),
                Type::parse_life,
            ),
            bytes::tag(""),
        )),
    );
    let (input, params) = parser(input)?;
    let params = params
        .iter()
        .map(|(mut span, mutable, ident, (typ, lifetimes), end)| {
            let begin = span.offset - orig_input.offset;
            let end = end.offset - orig_input.offset;
            span.fragment = &orig_input.fragment[begin..end];
            Param {
                typ: typ.clone(),
                mutable: mutable.fragment == "mut ",
                lifetimes: lifetimes.clone(),
                ident: ident.fragment,
                span,
            }
        })
        .collect();

    Ok((input, params))
}
