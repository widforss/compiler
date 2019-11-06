use super::{util, ErrorKind, Expr, IResult, Lifetimes, ParseError, Span, Statement, Stmt, Type};
use nom::{
    branch, bytes::complete as bytes, character::complete as character, multi, sequence, Err,
};

impl<'a> Stmt<'a> {
    pub fn parse(input: Span) -> IResult<Span, Stmt> {
        parse_block(input)
    }
}

impl Type {
    pub fn parse(input: Span) -> IResult<Span, Type> {
        parse_type(input)
    }
    pub fn parse_life(input: Span) -> IResult<Span, (Type, Lifetimes)> {
        let (input, (typ, mut lifetimes)) = parse_type_life(input)?;
        lifetimes.reverse();
        Ok((input, (typ, Lifetimes(lifetimes))))
    }
}

fn parse_stmt(input: Span) -> IResult<Span, Vec<Stmt>> {
    let (input, _) = character::multispace0(input)?;

    let parser = sequence::terminated(
        multi::many0(branch::alt((
            parse_block,
            parse_let,
            parse_while,
            parse_if_else,
            parse_return,
            parse_print,
            parse_assign,
            parse_fn,
        ))),
        character::multispace0,
    );
    match util::short(parser(input)) {
        Ok(res) => Ok(res),
        Err(Err::Failure(ParseError {
            input,
            error: ErrorKind::Nom(_),
            ..
        })) => {
            return Err(Err::Failure(ParseError::new(
                input,
                Some(input),
                ErrorKind::ParseStatement,
            )))
        }
        Err(err) => return Err(err),
    }
}

fn parse_block(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let (_, _) = bytes::tag("{")(input)?;
    let parser = sequence::tuple((
        sequence::delimited(bytes::tag("{"), parse_stmt, bytes::tag("}")),
        bytes::tag(""),
    ));
    let (input, (statements, end)) = util::short(parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Stmt {
            stmt: Statement::Block(statements),
            span,
        },
    ))
}

fn parse_fn(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let parser = sequence::tuple((
        Expr::parse,
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(";"))),
            bytes::tag(""),
        ),
    ));
    let (input, (expr, end)) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Stmt {
            stmt: Statement::Let(false, "!fn", Type::Unit, expr),
            span,
        },
    ))
}

fn parse_let(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let (input, _) = bytes::tag("let")(input)?;
    let parser = sequence::tuple((
        sequence::preceded(
            character::multispace1,
            branch::alt((
                sequence::terminated(bytes::tag("mut"), character::multispace1),
                bytes::tag(""),
            )),
        ),
        util::parse_ident_span,
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(":"))),
            Type::parse,
        ),
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag("="))),
            Expr::parse,
        ),
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(";"))),
            bytes::tag(""),
        ),
    ));
    let (input, (mutable, ident, typ, expr, end)) = util::short(parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    let mutable = mutable.fragment == "mut";

    Ok((
        input,
        Stmt {
            stmt: Statement::Let(mutable, ident.fragment, typ, expr),
            span,
        },
    ))
}

fn parse_while(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let (input, _) = bytes::tag("while")(input)?;
    let parser = sequence::tuple((
        branch::alt((
            Expr::parse_parens,
            sequence::preceded(character::multispace1, Expr::parse),
        )),
        parse_block,
        bytes::tag(""),
    ));
    let (input, (cond, body, end)) = util::short(parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Stmt {
            stmt: Statement::While(cond, Box::new(body)),
            span,
        },
    ))
}

fn parse_if_else(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let (input, _) = bytes::tag("if")(input)?;
    let parser = sequence::tuple((
        branch::alt((
            Expr::parse_parens,
            sequence::preceded(character::multispace1, Expr::parse),
        )),
        parse_block,
        multi::many_m_n(
            0,
            1,
            sequence::preceded(
                sequence::tuple((character::multispace0, bytes::tag("else"))),
                branch::alt((
                    parse_block,
                    sequence::preceded(character::multispace1, parse_if_else),
                )),
            ),
        ),
        bytes::tag(""),
    ));
    let (input, (cond, body, mut els, end)) = util::short(parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    let els = if let Some(statement) = els.pop() {
        statement
    } else {
        Stmt {
            stmt: Statement::Block(vec![]),
            span: end,
        }
    };

    Ok((
        input,
        Stmt {
            stmt: Statement::IfElse(cond, Box::new(body), Box::new(els)),
            span,
        },
    ))
}

fn parse_return(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let (input, _) = bytes::tag("return")(input)?;
    let parser = sequence::tuple((
        branch::alt((
            Expr::parse_parens,
            sequence::preceded(character::multispace1, Expr::parse),
        )),
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(";"))),
            bytes::tag(""),
        ),
    ));
    let (input, (expr, end)) = util::short(parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Stmt {
            stmt: Statement::Return(expr),
            span,
        },
    ))
}

fn parse_print(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let (input, _) = bytes::tag("print")(input)?;
    let parser = sequence::tuple((
        branch::alt((
            Expr::parse_parens,
            sequence::preceded(character::multispace1, Expr::parse),
        )),
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(";"))),
            bytes::tag(""),
        ),
    ));
    let (input, (expr, end)) = util::short(parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Stmt {
            stmt: Statement::Print(expr),
            span,
        },
    ))
}

fn parse_assign(input: Span) -> IResult<Span, Stmt> {
    let (input, _) = character::multispace0(input)?;
    let mut span_ident = input;
    let mut span = input;

    let ident_parser = sequence::tuple((
        multi::many0(sequence::tuple((bytes::tag("*"), character::multispace0))),
        util::parse_ident_span,
        bytes::tag(""),
        character::multispace0,
        bytes::tag("="),
    ));
    let expr_parser = sequence::tuple((
        Expr::parse,
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(";"))),
            bytes::tag(""),
        ),
    ));

    let (input, (_, _, end, _, _)) = ident_parser(input)?;
    span_ident.fragment = &span.fragment[..(end.offset - span.offset)];
    let (_, ident) = Expr::parse(span_ident)?;

    let (input, (expr, end)) = util::short(expr_parser(input))?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Stmt {
            stmt: Statement::Assign(ident, expr),
            span,
        },
    ))
}

fn parse_type(input: Span) -> IResult<Span, Type> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let parser_ref = branch::alt((
        sequence::tuple((
            bytes::tag("&"),
            sequence::preceded(
                character::multispace0,
                branch::alt((bytes::tag("mut "), bytes::tag(""))),
            ),
        )),
        sequence::tuple((bytes::tag(""), bytes::tag(""))),
    ));
    let (input, (pointer, mutable)) = parser_ref(input)?;
    if pointer.fragment == "&" {
        let mutable = mutable.fragment == "mut ";
        let (input, inner) = parse_type(input)?;
        return Ok((input, Type::Ref(mutable, Box::new(inner))));
    }

    parse_type_(input, orig_input)
}

fn parse_type_life(input: Span) -> IResult<Span, (Type, Vec<&str>)> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let parser_ref = branch::alt((
        sequence::tuple((
            bytes::tag("&"),
            sequence::preceded(character::multispace0, util::parse_lifetime),
            sequence::preceded(
                character::multispace0,
                branch::alt((bytes::tag("mut "), bytes::tag(""))),
            ),
        )),
        sequence::tuple((bytes::tag(""), bytes::tag(""), bytes::tag(""))),
    ));
    let (input, (pointer, lifetime, mutable)) = parser_ref(input)?;
    if pointer.fragment == "&" {
        let mutable = mutable.fragment == "mut ";
        let (input, (inner, mut inner_life)) = parse_type_life(input)?;

        let mut lifetimes = vec![lifetime.fragment];
        lifetimes.append(&mut inner_life);

        let typ = Type::Ref(mutable, Box::new(inner));

        return Ok((input, (typ, lifetimes)));
    }

    let (input, typ) = parse_type_(input, orig_input)?;
    Ok((input, (typ, vec![])))
}

fn parse_type_<'a>(input: Span<'a>, orig_input: Span<'a>) -> IResult<'a, Span<'a>, Type> {
    let parser = branch::alt((util::parse_ident_span, bytes::tag("()")));
    let (input, type_str) = parser(input)?;
    let typ = match type_str.fragment {
        "()" => Type::Unit,
        "bool" => Type::Bool,
        "i64" => Type::Int,
        "f64" => Type::Float,
        _ => {
            return Err(Err::Error(ParseError::new(
                orig_input,
                Some(type_str),
                ErrorKind::ParseType,
            )))
        }
    };
    Ok((input, typ))
}
