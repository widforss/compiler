use super::error::{Error, ErrorKind};
use super::{
    util,
    util::{IResult, Span},
};
use nom::{branch, bytes::complete::tag, character::complete as character, multi, sequence, Err};
use std::f64;

pub struct Expr<'a> {
    pub value: Value<'a>,
    pub span: Span<'a>,
}

pub enum Value<'a> {
    Literal(Literal),
    Func(Function<'a>, Vec<Expr<'a>>),
    Ident(&'a str),
}

#[derive(Debug)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
}

#[derive(Clone, Copy, Debug)]
pub enum Function<'a> {
    UnSub,
    UnInv,
    Pow,
    Mult,
    Rem,
    IntDiv,
    Div,
    Add,
    Sub,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    And,
    Or,
    Call(&'a str),
}

impl<'a> Expr<'a> {
    pub fn parse(input: Span<'a>) -> IResult<Span<'a>, Expr<'a>> {
        branch::alt((parse_infix, parse_expr_nobin))(input)
    }

    pub fn parse_parens(input: Span<'a>) -> IResult<Span<'a>, Expr<'a>> {
        parse_parens(input)
    }
}

const UNARYS: [Funcmap; 2] = [
    Funcmap {
        keyword: "-",
        prec: 8,         // Not used.
        ass: Ass::Right, // Not used.
        func: Function::UnSub,
    },
    Funcmap {
        keyword: "!",
        prec: 8,         // Not used.
        ass: Ass::Right, // Not used.
        func: Function::UnInv,
    },
];

const INFIXS: [Funcmap; 15] = [
    Funcmap {
        keyword: "**",
        prec: 7,
        ass: Ass::Right,
        func: Function::Pow,
    },
    Funcmap {
        keyword: "*",
        prec: 6,
        ass: Ass::Left,
        func: Function::Mult,
    },
    Funcmap {
        keyword: "%",
        prec: 6,
        ass: Ass::Left,
        func: Function::Rem,
    },
    Funcmap {
        keyword: "//",
        prec: 6,
        ass: Ass::Left,
        func: Function::IntDiv,
    },
    Funcmap {
        keyword: "/",
        prec: 6,
        ass: Ass::Left,
        func: Function::Div,
    },
    Funcmap {
        keyword: "+",
        prec: 5,
        ass: Ass::Left,
        func: Function::Add,
    },
    Funcmap {
        keyword: "-",
        prec: 5,
        ass: Ass::Left,
        func: Function::Sub,
    },
    Funcmap {
        keyword: "<=",
        prec: 4,
        ass: Ass::Left,
        func: Function::Leq,
    },
    Funcmap {
        keyword: ">=",
        prec: 4,
        ass: Ass::Left,
        func: Function::Geq,
    },
    Funcmap {
        keyword: "<",
        prec: 4,
        ass: Ass::Left,
        func: Function::Lt,
    },
    Funcmap {
        keyword: ">",
        prec: 4,
        ass: Ass::Left,
        func: Function::Gt,
    },
    Funcmap {
        keyword: "==",
        prec: 3,
        ass: Ass::Left,
        func: Function::Eq,
    },
    Funcmap {
        keyword: "!=",
        prec: 3,
        ass: Ass::Left,
        func: Function::Neq,
    },
    Funcmap {
        keyword: "&&",
        prec: 2,
        ass: Ass::Left,
        func: Function::And,
    },
    Funcmap {
        keyword: "||",
        prec: 1,
        ass: Ass::Left,
        func: Function::Or,
    },
];

struct Funcmap {
    keyword: &'static str,
    prec: u8,
    ass: Ass,
    func: Function<'static>,
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
    branch::alt((parse_bool, parse_number))(input)
}

fn parse_parens(input: Span) -> IResult<Span, Expr> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let (_, _) = tag("(")(input)?;
    let parser = sequence::delimited(tag("("), Expr::parse, tag(")"));
    match util::short(parser(input)) {
        Ok(res) => Ok(res),
        Err(Err::Failure(Error { error, .. })) => Err(Err::Failure(Error::new(
            orig_input,
            Some(orig_input),
            error,
        ))),
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
            value: Value::Func(Function::Call(ident), args),
            span: span,
        },
    ))
}

fn parse_ident(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = util::parse_ident_span(input)?;
    Ok((
        input,
        Expr {
            value: Value::Ident(ident.fragment),
            span: ident,
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
        branch::alt((tag::<&str, Span, Error>("-"), tag(""))),
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
        Err(Err::Error(Error::new(
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
            Err(_) => Err(Err::Failure(Error::new(
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
            Err(_) => Err(Err::Failure(Error::new(
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
    let (input, (func_map, right, end)) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Expr {
            value: Value::Func(func_map.func, vec![right]),
            span,
        },
    ))
}

fn parse_infix(input: Span) -> IResult<Span, Expr> {
    // Initialize with minimum precedence 1.
    parse_infix_un(input, 1)
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
    // See if we can find an infix function. If not, return what we have.
    let (input_bin, _) = character::multispace0(input_bin)?;
    let mut span = orig_input;

    let (input, func_map) = match tag_infix(input_bin) {
        Ok(res) => res,
        Err(Err::Error(Error { input, .. })) => return Ok((input, left)),
        Err(err) => return Err(err),
    };

    // Does the new infix fulfill our precedence criteria?
    if func_map.prec < min_prec {
        return Ok((input_bin, left));
    }

    // Parse the right-hand-side.
    let new_min_prec = match func_map.ass {
        Ass::Left => func_map.prec + 1,
        Ass::Right => func_map.prec,
    };
    // Do not allow unary operators on rhs of arithmetic operations.
    let right_parser = match func_map.keyword {
        "**" | "*" | "/" | "//" | "%" | "+" | "-" => parse_infix_noun,
        _ => parse_infix_un,
    };
    let (input, right) = right_parser(input, new_min_prec)?;

    let spanlen = right.span.offset - span.offset + right.span.fragment.len();
    span.fragment = &span.fragment[..spanlen];

    // Put together the infix function with arguments. Continue forward.
    let expr = Expr {
        value: Value::Func(func_map.func, vec![left, right]),
        span,
    };
    parse_infix_left(orig_input, input, min_prec, expr)
}

fn tag_unary<'a>(input: Span<'a>) -> IResult<'a, Span<'a>, &'a Funcmap> {
    tag_func(input, &UNARYS)
}
fn tag_infix<'a>(input: Span<'a>) -> IResult<'a, Span<'a>, &'a Funcmap> {
    tag_func(input, &INFIXS)
}
fn tag_func<'a>(input: Span<'a>, funcs: &'a [Funcmap]) -> IResult<'a, Span<'a>, &'a Funcmap> {
    for func_map in funcs.iter() {
        match tag(func_map.keyword)(input) {
            Ok((input, _)) => return Ok((input, &func_map)),
            Err(Err::Error(_)) => (),
            Err(err) => return Err(err),
        }
    }
    Err(Err::Error(Error::new(
        input,
        None,
        ErrorKind::Nom(nom::error::ErrorKind::Tag),
    )))
}

impl<'a> Value<'a> {
    pub fn print(&self, indent: usize) -> String {
        let instr = "    ".repeat(indent);
        match self {
            Value::Literal(val) => format!("{:?}", val),
            Value::Func(func, exprs) => {
                let mut string = format!("{} [\n", func);
                for expr in exprs.iter() {
                    string = format!("{}{}    {},\n", string, instr, expr.value.print(indent + 1));
                }
                format!("{}{}]", string, instr)
            }
            Value::Ident(str) => format!("Ident({})", str),
        }
    }
}