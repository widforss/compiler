mod error;
mod expr;
mod stmt;
mod util;

use super::{Ast, Func, Mutability, Span, Stmt, Type};
pub use error::Error;
use error::{ErrorKind, IResult};
use nom::{
    branch, bytes::complete as bytes, character::complete as character, multi, sequence, Err,
};
use std::collections::HashMap;

impl<'a> Ast<'a> {
    pub fn parse(input: &'a str) -> Result<Self, Error> {
        match parse_fn(Span::new(input)) {
            Ok((Span { fragment: "", .. }, tree)) => Ok(tree),
            Ok((input, _)) => Err(Error::new(input, Some(input), ErrorKind::NotRecognised)),
            Err(Err::Failure(Error { input, span, error })) => Err(Error::new(input, span, error)),
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
            sequence::delimited(bytes::tag("("), parse_params, bytes::tag(")")),
        ),
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag("->"))),
            Type::parse,
        ),
        Stmt::parse,
        character::multispace0,
    )));
    let (input, mut functions) = match util::short(parser(input)) {
        Ok(res) => res,
        Err(Err::Failure(Error {
            input,
            error: ErrorKind::Nom(_),
            ..
        })) => {
            return Err(Err::Failure(Error::new(
                input,
                Some(input),
                ErrorKind::ParseFunction,
            )))
        }
        Err(err) => return Err(err),
    };

    let mut funcmap = HashMap::new();
    functions.reverse();
    loop {
        if let Some((mut span, ident, params, typ, body, end)) = functions.pop() {
            let begin = span.offset - orig_input.offset;
            let end = end.offset - orig_input.offset;
            span.fragment = &orig_input.fragment[begin..end];

            if !funcmap.contains_key(ident.fragment) {
                funcmap.insert(
                    ident.fragment,
                    Func {
                        typ,
                        params,
                        body,
                        span,
                    },
                );
            } else {
                return Err(Err::Failure(Error::new(
                    orig_input,
                    Some(span),
                    ErrorKind::DoubleFunctionDecl,
                )));
            }
        } else {
            break;
        }
    }

    let mainfunc = match funcmap.get("main") {
        Some(res) => res,
        None => {
            return Err(Err::Failure(Error::new(
                orig_input,
                None,
                ErrorKind::NoMain,
            )))
        }
    };
    match mainfunc.typ {
        Type::Unit => Ok((input, Ast(funcmap))),
        _ => Err(Err::Failure(Error::new(
            orig_input,
            Some(mainfunc.span),
            ErrorKind::MainReturns,
        ))),
    }
}

fn parse_params(input: Span) -> IResult<Span, Vec<(Type, Mutability, Span)>> {
    let (input, _) = character::multispace0(input)?;

    let parser = multi::separated_list(
        sequence::preceded(character::multispace0, bytes::tag(",")),
        sequence::tuple((
            sequence::preceded(
                character::multispace0,
                branch::alt((bytes::tag("mut "), bytes::tag(""))),
            ),
            util::parse_ident_span,
            sequence::preceded(
                sequence::tuple((character::multispace0, bytes::tag(":"))),
                Type::parse,
            ),
        )),
    );
    let (input, params) = parser(input)?;
    let params = params
        .iter()
        .map(|(mutable, span, typ)| (*typ, mutable.fragment == "mut ", *span))
        .collect();

    Ok((input, params))
}
