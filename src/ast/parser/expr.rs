use super::{BinOp, Expr, Literal, Span, UnOp, Value, ParseError, ErrorKind, IResult, util};
use nom::{branch, bytes::complete::tag, character::complete as character, multi, sequence, Err};
use std::f64;

impl<'a> Expr<'a> {
    pub fn parse(input: Span<'a>) -> IResult<Span<'a>, Expr<'a>> {
        branch::alt((parse_infix, parse_expr_nobin))(input)
    }

    pub fn parse_parens(input: Span<'a>) -> IResult<Span<'a>, Expr<'a>> {
        parse_parens(input)
    }
}

const UNARYS: [UnOpmap; 2] = [
    UnOpmap {
        keyword: "-",
        op: UnOp::Sub,
    },
    UnOpmap {
        keyword: "!",
        op: UnOp::Inv,
    },
];

const INFIXS: [BinOpmap; 15] = [
    BinOpmap {
        keyword: "**",
        prec: 7,
        ass: Ass::Right,
        op: BinOp::Pow,
    },
    BinOpmap {
        keyword: "*",
        prec: 6,
        ass: Ass::Left,
        op: BinOp::Mult,
    },
    BinOpmap {
        keyword: "%",
        prec: 6,
        ass: Ass::Left,
        op: BinOp::Rem,
    },
    BinOpmap {
        keyword: "//",
        prec: 6,
        ass: Ass::Left,
        op: BinOp::IntDiv,
    },
    BinOpmap {
        keyword: "/",
        prec: 6,
        ass: Ass::Left,
        op: BinOp::Div,
    },
    BinOpmap {
        keyword: "+",
        prec: 5,
        ass: Ass::Left,
        op: BinOp::Add,
    },
    BinOpmap {
        keyword: "-",
        prec: 5,
        ass: Ass::Left,
        op: BinOp::Sub,
    },
    BinOpmap {
        keyword: "<=",
        prec: 4,
        ass: Ass::Left,
        op: BinOp::Leq,
    },
    BinOpmap {
        keyword: ">=",
        prec: 4,
        ass: Ass::Left,
        op: BinOp::Geq,
    },
    BinOpmap {
        keyword: "<",
        prec: 4,
        ass: Ass::Left,
        op: BinOp::Lt,
    },
    BinOpmap {
        keyword: ">",
        prec: 4,
        ass: Ass::Left,
        op: BinOp::Gt,
    },
    BinOpmap {
        keyword: "==",
        prec: 3,
        ass: Ass::Left,
        op: BinOp::Eq,
    },
    BinOpmap {
        keyword: "!=",
        prec: 3,
        ass: Ass::Left,
        op: BinOp::Neq,
    },
    BinOpmap {
        keyword: "&&",
        prec: 2,
        ass: Ass::Left,
        op: BinOp::And,
    },
    BinOpmap {
        keyword: "||",
        prec: 1,
        ass: Ass::Left,
        op: BinOp::Or,
    },
];

struct BinOpmap {
    keyword: &'static str,
    prec: u8,
    ass: Ass,
    op: BinOp,
}

struct UnOpmap {
    keyword: &'static str,
    op: UnOp,
}

enum Ass {
    Left,
    Right,
}

fn parse_expr_nobin(input: Span) -> IResult<Span, Expr> {
    branch::alt((parse_expr_nobin_noun, parse_unary))(input)
}

fn parse_expr_nobin_noun(input: Span) -> IResult<Span, Expr> {
    branch::alt((parse_parens, parse_literal, parse_call, parse_ident))(input)
}

fn parse_literal(input: Span) -> IResult<Span, Expr> {
    // More primitive type categories go here (branch::alt() if > 1):
    branch::alt((parse_unit, parse_bool, parse_number))(input)
}

fn parse_parens(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let (_, _) = tag("(")(input)?;
    let parser = sequence::delimited(tag("("), Expr::parse, tag(")"));
    match parser(input) {
        Ok(res) => Ok(res),
        Err(Err::Failure(ParseError { error, .. })) => Err(Err::Failure(ParseError::new(
            orig_input,
            Some(orig_input),
            error,
        ))),
        Err(Err::Error(ParseError { error, .. })) => {
            Err(Err::Error(ParseError::new(orig_input, Some(orig_input), error)))
        }
        Err(_) => panic!(),
    }
}

fn parse_call(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let args_parser = multi::separated_list(
        sequence::preceded(character::multispace0, tag(",")),
        Expr::parse,
    );
    let parser = sequence::tuple((
        parse_ident,
        sequence::preceded(
            character::multispace0,
            sequence::delimited(tag("("), args_parser, tag(")")),
        ),
        tag(""),
    ));
    let (input, (ident, args, end)) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    let ident = match ident.value {
        Value::Ident(str) => str,
        _ => panic!(),
    };

    Ok((
        input,
        Expr {
            value: Value::Call(ident, args),
            span: span,
        },
    ))
}

fn parse_ident(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = util::parse_ident_span(input)?;
    Ok((
        input,
        Expr {
            value: Value::Ident(ident),
            span: ident,
        },
    ))
}

fn parse_unit(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let parser = sequence::preceded(tag("()"), tag(""));
    let (input, end) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];
    Ok((
        input,
        Expr {
            value: Value::Literal(Literal::Unit),
            span,
        },
    ))
}

fn parse_bool(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let parser = sequence::tuple((
        branch::alt((tag("!"), tag(""))),
        sequence::preceded(
            character::multispace0,
            branch::alt((tag("true"), tag("false"))),
        ),
        tag(""),
    ));
    let (input, (inv, bool_str, end)) = parser(input)?;
    let bool = match (inv.fragment, bool_str.fragment) {
        ("", "true") | ("!", "false") => true,
        ("", "false") | ("!", "true") => false,
        _ => panic!(),
    };
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Expr {
            value: Value::Literal(Literal::Bool(bool)),
            span,
        },
    ))
}

fn parse_number(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    branch::alt((parse_number_special, parse_number_normal))(input)
}
fn parse_number_special(input: Span) -> IResult<Span, Expr> {
    let mut span = input;

    let infty_parser = sequence::tuple((
        branch::alt((tag::<&str, Span, ParseError>("-"), tag(""))),
        sequence::preceded(
            character::multispace0,
            branch::alt((tag("INFINITY"), tag("NAN"))),
        ),
        tag(""),
    ));
    if let Ok((input, (sign, val, end))) = infty_parser(input) {
        let special_value = match (sign.fragment, val.fragment) {
            ("", "INFINITY") => f64::INFINITY,
            ("-", "INFINITY") => f64::NEG_INFINITY,
            (_, "NAN") => f64::NAN,
            _ => panic!(),
        };
        span.fragment = &span.fragment[..(end.offset - span.offset)];

        Ok((
            input,
            Expr {
                span,
                value: Value::Literal(Literal::Float(special_value)),
            },
        ))
    } else {
        Err(Err::Error(ParseError::new(
            input,
            Some(span),
            ErrorKind::ParseFloat,
        )))
    }
}
fn parse_number_normal(input: Span) -> IResult<Span, Expr> {
    let mut span = input;

    let numparser = sequence::tuple((
        branch::alt((tag("-"), tag(""))),
        sequence::preceded(character::multispace0, character::digit1),
        branch::alt((tag("."), tag(""))),
        character::digit0,
        tag(""),
    ));
    let (input, (sign, int, dot, _, end)) = numparser(input)?;

    span.fragment = &span.fragment[..(end.offset - span.offset)];
    let number_str = &span.fragment[(int.offset - span.offset)..];
    let number_str = format!("{}{}", sign.fragment, number_str);

    if dot.fragment.len() == 1 {
        match number_str.parse::<f64>() {
            Ok(float) => Ok((
                input,
                Expr {
                    span,
                    value: Value::Literal(Literal::Float(float)),
                },
            )),
            Err(_) => Err(Err::Failure(ParseError::new(
                input,
                Some(span),
                ErrorKind::ParseFloat,
            ))),
        }
    } else {
        match number_str.parse::<i64>() {
            Ok(int) => Ok((
                input,
                Expr {
                    span,
                    value: Value::Literal(Literal::Int(int)),
                },
            )),
            Err(_) => Err(Err::Failure(ParseError::new(
                input,
                Some(span),
                ErrorKind::ParseInt,
            ))),
        }
    }
}

fn parse_unary(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    let mut span = input;

    let parser = sequence::tuple((tag_unary, parse_expr_nobin_noun, tag("")));
    let (input, (op_map, right, end)) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Expr {
            value: Value::UnOp(op_map.op, Box::new(right)),
            span,
        },
    ))
}

fn parse_infix(input: Span) -> IResult<Span, Expr> {
    // Initialize with minimum precedence 1.
    match parse_infix_un(input, 1) {
        Ok(res) => Ok(res),
        Err(Err::Error(ParseError { span, error, .. })) =>
            Err(Err::Error(ParseError { input, span, error })),
        Err(Err::Failure(ParseError { span, error, .. })) => {
            Err(Err::Failure(ParseError { input, span, error }))
        }
        _ => panic!(),
    }
}
fn parse_infix_un<'a>(orig_input: Span, min_prec: u8) -> IResult<Span, Expr> {
    // First, find left hand side expression. Search for everything but BinOps.
    let (orig_input, _) = character::multispace0(orig_input)?;
    let (input, left) = parse_expr_nobin(orig_input)?;
    parse_infix_left(orig_input, input, min_prec, left)
}
fn parse_infix_noun<'a>(orig_input: Span, min_prec: u8) -> IResult<Span, Expr> {
    // Almost identical to parse_infix_un(), but we do not accept unary ops.
    let (orig_input, _) = character::multispace0(orig_input)?;
    let (input, left) = parse_expr_nobin_noun(orig_input)?;
    parse_infix_left(orig_input, input, min_prec, left)
}
fn parse_infix_left<'a>(
    orig_input: Span<'a>,
    input_bin: Span<'a>,
    min_prec: u8,
    left: Expr<'a>,
) -> IResult<'a, Span<'a>, Expr<'a>> {
    // See if we can find an infix option. If not, return what we have.
    let (input_bin, _) = character::multispace0(input_bin)?;
    let mut span = orig_input;

    let (input, op_map) = match tag_infix(input_bin) {
        Ok(res) => res,
        Err(Err::Error(ParseError { input, .. })) => return Ok((input, left)),
        Err(err) => return Err(err),
    };

    // Does the new infix fulfill our precedence criteria?
    if op_map.prec < min_prec {
        return Ok((input_bin, left));
    }

    // Parse the right-hand-side.
    let new_min_prec = match op_map.ass {
        Ass::Left => op_map.prec + 1,
        Ass::Right => op_map.prec,
    };
    // Do not allow unary operators on rhs of arithmetic operations.
    let right_parser = match op_map.keyword {
        "**" | "*" | "/" | "//" | "%" | "+" | "-" => parse_infix_noun,
        _ => parse_infix_un,
    };
    let (input, right) = right_parser(input, new_min_prec)?;

    let spanlen = right.span.offset - span.offset + right.span.fragment.len();
    span.fragment = &span.fragment[..spanlen];

    // Put together the infix option with arguments. Continue forward.
    let expr = Expr {
        value: Value::BinOp(op_map.op, Box::new(left), Box::new(right)),
        span,
    };
    parse_infix_left(orig_input, input, min_prec, expr)
}

fn tag_unary<'a>(input: Span<'a>) -> IResult<'a, Span<'a>, &'a UnOpmap> {
    for op_map in UNARYS.iter() {
        match tag(op_map.keyword)(input) {
            Ok((input, _)) => return Ok((input, &op_map)),
            Err(Err::Error(_)) => (),
            Err(err) => return Err(err),
        }
    }
    Err(Err::Error(ParseError::new(
        input,
        None,
        ErrorKind::Nom(nom::error::ErrorKind::Tag),
    )))
}

fn tag_infix<'a>(input: Span<'a>) -> IResult<'a, Span<'a>, &'a BinOpmap> {
    for op_map in INFIXS.iter() {
        match tag(op_map.keyword)(input) {
            Ok((input, _)) => return Ok((input, &op_map)),
            Err(Err::Error(_)) => (),
            Err(err) => return Err(err),
        }
    }
    Err(Err::Error(ParseError::new(
        input,
        None,
        ErrorKind::Nom(nom::error::ErrorKind::Tag),
    )))
}
