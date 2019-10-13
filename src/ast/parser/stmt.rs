use super::{Expr, Span, Statement, Stmt, Type, ParseError, ErrorKind, IResult, util};
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
            parse_type,
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
            stmt: Statement::Let(mutable, ident, typ, expr),
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
    let mut span = input;

    let ident_parser = sequence::terminated(
        util::parse_ident_span,
        sequence::tuple((character::multispace0, bytes::tag("="))),
    );
    let expr_parser = sequence::tuple((
        Expr::parse,
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag(";"))),
            bytes::tag(""),
        ),
    ));
    let (input, ident) = ident_parser(input)?;
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
