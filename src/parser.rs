mod error;
mod expr;
mod stmt;
mod util;

pub use error::{Error, ErrorKind};
use expr::Function;
use stmt::{Stmt, Type};
use util::{IResult, Span};
use nom::{
    bytes::complete as bytes, character::complete as character, multi, sequence, Err,
};
use std::collections::HashMap;
use std::fmt;

pub struct Ast<'a>(HashMap<&'a str, Func<'a>>);

pub struct Func<'a> {
    typ: Type,
    params: Vec<(Type, &'a str)>,
    body: Stmt<'a>,
    span: Span<'a>,
}

impl<'a> Ast<'a> {
    pub fn parse(input: &'a str) -> Result<Self, Error> {
        match parse_fn(Span::new(input)) {
            Ok((Span { fragment: "", .. }, tree)) => Ok(tree),
            Ok((input, _)) => Err(Error::new(input, Some(input), ErrorKind::NotRecognised)),
            Err(Err::Failure(Error {
                input,
                span: Some(span),
                error,
            })) => Err(Error::new(input, Some(span), error)),
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

    Ok((input, Ast(funcmap)))
}

fn parse_params(input: Span) -> IResult<Span, Vec<(Type, &str)>> {
    let (input, _) = character::multispace0(input)?;

    let parser = multi::separated_list(
        sequence::preceded(character::multispace0, bytes::tag(",")),
        sequence::tuple((
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
        .map(|(span, typ)| (*typ, span.fragment))
        .collect();

    Ok((input, params))
}

impl<'a> fmt::Display for Ast<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Ast(functions) = self;
        for (name, function) in functions.iter() {
            write!(f, "Ident({}): {}\n", name, function)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Func<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut paramstr = String::from("[\n");
        for (typ, ident) in self.params.iter() {
            paramstr = format!(
                "{}        {{\n            \
                 Type: {:?},\n            \
                 Identifier: Ident({}),\n        \
                 }},\n",
                paramstr, typ, ident,
            );
        }
        paramstr = format!("{}    ]", paramstr);
        write!(
            f,
            "{{\n    \
             Type: {:?},\n    \
             Parameters: {},\n    \
             Function: {},\n\
             }},",
            self.typ,
            paramstr,
            self.body.stmt.block(1)
        )
    }
}

impl<'a> fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Call(func) => write!(f, "Call(Ident({}))", func),
            _ => write!(f, "{:?}", self),
        }
    }
}
