use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{digit0, digit1, multispace0},
    error, sequence, Err,
};
use nom_locate::LocatedSpan;
use std::f64;

const INPUT: &str = " - \n\t(-2.54 +(-INFINITY - 9)) ** (-0. ) * ( 3 //5 ) - 4 ";
//const INPUT: &str = "3 > 4 != 3 < 3 || ! false";
//const INPUT: &str = "-2+3**2*3/5-4";
//const INPUT: &str = "\n\n\t2+  - 3";
//const INPUT: &str = "2+30000000000000000000000";
//const INPUT: &str = "2";
//const INPUT: &str = "30000000000000000000000";
//const INPUT: &str = "3+2a";
//const INPUT: &str = "-(2+3e)";

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

#[derive(Debug)]
struct Expr<'a> {
    value: Value<'a>,
    span: Span<'a>,
}

#[derive(Debug)]
enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
}

#[derive(Debug)]
enum Value<'a> {
    Literal(Literal),
    Func(Function, Vec<Expr<'a>>),
}

#[derive(Clone, Copy, Debug)]
enum Function {
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
}

struct Funcmap {
    keyword: &'static str,
    prec: u8,
    ass: Ass,
    func: Function,
}

enum Ass {
    Left,
    Right,
}

type Span<'a> = LocatedSpan<&'a str>;
type SpanFuncmap<'a> = (Span<'a>, &'a Funcmap);
type IResult<'a, I, O, E = Error<'a>> = Result<(I, O), Err<E>>;

fn main() {
    match parse(Span::new(INPUT)) {
        Ok(tree) => {
            println!("{:#?}", tree);
        }
        Err(Error {
            span: Some(span),
            error,
            ..
        }) => println!(
            "{} at line {}, column {}:\n\t{}",
            error.description(),
            span.line,
            span.get_utf8_column(),
            span.fragment,
        ),
        Err(err) => panic!(err),
    }
}

fn parse<'a>(input: Span) -> Result<Expr, Error> {
    match parse_expr(input) {
        Ok((Span { fragment: "", .. }, tree)) => Ok(tree),
        Ok((input, _)) => Err(Error {
            input,
            span: Some(input),
            error: ErrorKind::NotRecognised,
        }),
        Err(Err::Incomplete(_)) => Err(Error {
            input: input,
            span: Some(input),
            error: ErrorKind::Incomplete,
        }),
        Err(Err::Error(err)) => Err(err),
        Err(Err::Failure(err)) => Err(err),
    }
}

fn parse_expr<'a>(input: Span) -> IResult<Span, Expr> {
    branch::alt((parse_infix, parse_expr_nobin))(input)
}

fn parse_expr_nobin<'a>(input: Span) -> IResult<Span, Expr> {
    branch::alt((parse_expr_nobin_noun, parse_unary))(input)
}

fn parse_expr_nobin_noun<'a>(input: Span) -> IResult<Span, Expr> {
    branch::alt((parse_parens, parse_literal))(input)
}

fn parse_parens(input: Span) -> IResult<Span, Expr> {
    let (input, _) = multispace0(input)?;
    let parser = sequence::delimited(tag("("), parse_expr, tag(")"));
    let (input, content) = parser(input)?;
    Ok((input, content))
}

fn parse_literal(input: Span) -> IResult<Span, Expr> {
    // More primitive type categories go here (branch::alt() if > 1):
    branch::alt((parse_bool, parse_number))(input)
}

fn parse_bool(input: Span) -> IResult<Span, Expr> {
    let (orig_input, _) = multispace0(input)?;

    let parser = sequence::tuple((
        tag(""),
        branch::alt((tag("!"), tag(""))),
        multispace0,
        branch::alt((tag("true"), tag("false"))),
        tag(""),
    ));
    let (input, (mut span, inv, _, bool_str, end)) = parser(orig_input)?;
    let bool = match (inv.fragment, bool_str.fragment) {
        ("", "true") | ("!", "false") => true,
        ("", "false") | ("!", "true") => false,
        _ => panic!(),
    };

    // We cannot create a new &str in span, due to how Span uses it internally!
    span.fragment = &orig_input.fragment[..(end.offset - span.offset)];

    Ok((
        input,
        Expr {
            span,
            value: Value::Literal(Literal::Bool(bool)),
        },
    ))
}

fn parse_number(input: Span) -> IResult<Span, Expr> {
    let (orig_input, _) = multispace0(input)?;

    // Parse float special values.
    let infty_parser = sequence::tuple((
        tag(""),
        tag("-"),
        multispace0,
        branch::alt((tag("INFINITY"), tag("NEG_INFINITY"), tag("NAN"))),
        tag(""),
    ));
    let orig_input = match infty_parser(orig_input) {
        Ok((input, (mut span, sign, _, val, end))) => {
            let special_value = match (sign.fragment, val.fragment) {
                ("", "INFINITY") | ("-", "NEG_INFINITY") => f64::INFINITY,
                ("", "NEG_INFINITY") | ("-", "INFINITY") => f64::NEG_INFINITY,
                (_, "NAN") => f64::NAN,
                _ => panic!(),
            };
            span.fragment = &orig_input.fragment[..(end.offset - span.offset)];
            return Ok((
                input,
                Expr {
                    span,
                    value: Value::Literal(Literal::Float(special_value)),
                },
            ));
        }
        Err(Err::Error(Error { input, .. })) => input,
        Err(err) => return Err(err),
    };

    let numparser = sequence::tuple((
        tag(""),
        branch::alt((tag("+"), tag("-"), tag(""))),
        multispace0,
        digit1,
        branch::alt((tag("."), tag(""))),
        digit0,
        tag(""),
    ));
    let (input, (mut span, sign, _, int, dot, _, end)) = numparser(orig_input)?;

    // We cannot create a new &str in span, due to how Span uses it internally!
    span.fragment = &orig_input.fragment[..(end.offset - span.offset)];
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
            Err(err) => Err(Err::Failure(Error {
                input,
                span: Some(span),
                error: ErrorKind::ParseFloat(err),
            })),
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
            Err(err) => Err(Err::Failure(Error {
                input,
                span: Some(span),
                error: ErrorKind::ParseInt(err),
            })),
        }
    }
}

fn parse_unary(input: Span) -> IResult<Span, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, (span, func_map)) = tag_func(input, &UNARYS)?;
    let (input, right) = match parse_expr_nobin_noun(input) {
        Ok(res) => res,
        Err(Err::Error(_)) => {
            return Err(Err::Failure(Error {
                input,
                span: Some(input),
                error: ErrorKind::NotRecognised,
            }))
        }
        Err(err) => return Err(err),
    };
    Ok((
        input,
        Expr {
            span,
            value: Value::Func(func_map.func, vec![right]),
        },
    ))
}

fn parse_infix(input: Span) -> IResult<Span, Expr> {
    // Initialize with minimum precedence 1.
    parse_infix_un(input, 1)
}
fn parse_infix_un<'a>(input: Span, min_prec: u8) -> IResult<Span, Expr> {
    // First, find left hand side expression. Search for everything but BinOps.
    let (input, left) = parse_expr_nobin(input)?;
    parse_infix_left(input, min_prec, left)
}
fn parse_infix_noun<'a>(input: Span, min_prec: u8) -> IResult<Span, Expr> {
    // Almost identical to parse_infix_un(), but we do not accept unary ops.
    let (input, left) = parse_expr_nobin_noun(input)?;
    parse_infix_left(input, min_prec, left)
}
fn parse_infix_left<'a>(
    input_bin: Span<'a>,
    min_prec: u8,
    left: Expr<'a>,
) -> IResult<'a, Span<'a>, Expr<'a>> {
    // See if we can find an infix function. If not, return what we have.
    let (input_bin, _) = multispace0(input_bin)?;
    let (input, (span, func_map)) = match tag_func(input_bin, &INFIXS) {
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
    let (input, right) = match func_map.keyword {
        "**" | "*" | "/" | "//" | "%" | "+" | "-" => parse_infix_noun(input, new_min_prec)?,
        _ => parse_infix_un(input, new_min_prec)?,
    };

    // Put together the infix function with arguments. Continue forward.
    let expr = Expr {
        span,
        value: Value::Func(func_map.func, vec![left, right]),
    };
    parse_infix_left(input, min_prec, expr)
}

fn tag_func<'a>(input: Span<'a>, funcs: &'a [Funcmap]) -> IResult<'a, Span<'a>, SpanFuncmap<'a>> {
    for func_map in funcs.iter() {
        match tag(func_map.keyword)(input) {
            Ok((input, span)) => return Ok((input, (span, &func_map))),
            Err(Err::Error(_)) => (),
            Err(err) => return Err(err),
        }
    }
    Err(Err::Error(Error {
        input,
        span: None,
        error: ErrorKind::Nom(error::ErrorKind::Tag),
    }))
}

struct Error<'a> {
    input: Span<'a>,
    span: Option<Span<'a>>,
    error: ErrorKind,
}

enum ErrorKind {
    NotRecognised,
    Incomplete,
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    Nom(error::ErrorKind),
}

const ERR_MSG_NOTRECOGNISED: &str = "Failed to parse input";
const ERR_MSG_INCOMPLETE: &str = "There was not enough data";
const ERR_MSG_PARSEINT: &str = "Could not parse integer";
const ERR_MSG_PARSEFLOAT: &str = "Could not parse float";
impl ErrorKind {
    fn description(&self) -> &str {
        match self {
            ErrorKind::NotRecognised => ERR_MSG_NOTRECOGNISED,
            ErrorKind::Incomplete => ERR_MSG_INCOMPLETE,
            ErrorKind::ParseInt(_) => ERR_MSG_PARSEINT,
            ErrorKind::ParseFloat(_) => ERR_MSG_PARSEFLOAT,
            ErrorKind::Nom(err) => err.description(),
        }
    }
}

impl<'a> error::ParseError<Span<'a>> for Error<'a> {
    fn from_error_kind(input: Span<'a>, kind: error::ErrorKind) -> Self {
        Error {
            input,
            span: None,
            error: ErrorKind::Nom(kind),
        }
    }

    fn append(_: Span<'a>, _: error::ErrorKind, other: Self) -> Self {
        other
    }
}
