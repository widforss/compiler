use nom::{branch, bytes::complete::tag, character::complete::digit1, error, Err};
use nom_locate::LocatedSpan;

const INPUT: &str = "-2+3**2*3/5-4";
//const INPUT: &str = "2+-3";
//const INPUT: &str = "2+30000000000000000000000";
//const INPUT: &str = "2";
//const INPUT: &str = "30000000000000000000000";
//const INPUT: &str = "3+2a";

const UNARYS: [Funcmap; 1] = [Funcmap {
    keyword: "-",
    prec: 4,
    ass: Ass::Right,
    func: Function::UnSub,
}];

const INFIXS: [Funcmap; 5] = [
    Funcmap {
        keyword: "**",
        prec: 3,
        ass: Ass::Right,
        func: Function::Pow,
    },
    Funcmap {
        keyword: "*",
        prec: 2,
        ass: Ass::Left,
        func: Function::Mult,
    },
    Funcmap {
        keyword: "/",
        prec: 2,
        ass: Ass::Left,
        func: Function::Div,
    },
    Funcmap {
        keyword: "+",
        prec: 1,
        ass: Ass::Left,
        func: Function::Add,
    },
    Funcmap {
        keyword: "-",
        prec: 1,
        ass: Ass::Left,
        func: Function::Sub,
    },
];

struct Expr<'a> {
    span: Span<'a>,
    val: Value<'a>,
}

enum Value<'a> {
    Int(i32),
    UnFunc(Function, Box<Expr<'a>>),
    Func(Function, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Clone, Copy)]
enum Function {
    UnSub,
    Pow,
    Mult,
    Div,
    Add,
    Sub,
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
            println!("{:#?}", SimpleExpr::new(&tree));
        }
        Err(Error {
            val: Some(val),
            error,
            ..
        }) => println!(
            "{:#?} at line {}, column {}:\n\t{}",
            error.description(),
            val.line,
            val.get_utf8_column(),
            val.fragment,
        ),
        Err(err) => panic!(err),
    }
}

fn parse<'a>(input: Span) -> Result<Expr, Error> {
    match parse_expr(input) {
        Ok((Span { fragment: "", .. }, tree)) => Ok(tree),
        Ok((input, _)) => Err(Error {
            input: input,
            val: Some(input),
            error: ErrorKind::NotRecognised,
        }),
        Err(Err::Incomplete(_)) => Err(Error {
            input: input,
            val: Some(input),
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
    branch::alt((parse_unary, parse_expr_nobin_noun))(input)
}

fn parse_expr_nobin_noun<'a>(input: Span) -> IResult<Span, Expr> {
    parse_value(input)
}

fn parse_value(input: Span) -> IResult<Span, Expr> {
    // More primitive type categories go here (branch::alt() if > 1):
    parse_number(input)
}

fn parse_number(input: Span) -> IResult<Span, Expr> {
    // More number types go here (branch::alt() if > 1):
    parse_int(input)
}

fn parse_int(input: Span) -> IResult<Span, Expr> {
    let (input, digits) = digit1(input)?;
    let int = match digits.fragment.parse() {
        Ok(int) => int,
        Err(err) => {
            return Err(Err::Failure(Error {
                input,
                val: Some(digits),
                error: ErrorKind::ParseInt(err),
            }))
        }
    };
    Ok((
        input,
        Expr {
            span: digits,
            val: Value::Int(int),
        },
    ))
}

fn parse_unary(input: Span) -> IResult<Span, Expr> {
    let (input, (span, func_map)) = tag_unary(input)?;
    let (input, right) = parse_expr_nobin(input)?;
    Ok((
        input,
        Expr {
            span,
            val: Value::UnFunc(func_map.func, Box::new(right)),
        },
    ))
}

fn parse_infix(input: Span) -> IResult<Span, Expr> {
    // Initialize with minimum precedence 1.
    parse_infix_first(input, 1)
}
fn parse_infix_first<'a>(input: Span, min_prec: u8) -> IResult<Span, Expr> {
    // First, find left hand side expression. Search for everything but BinOps.
    let (input, left) = parse_expr_nobin(input)?;
    parse_infix_left(input, min_prec, left)
}
fn parse_infix_prec<'a>(input: Span, min_prec: u8) -> IResult<Span, Expr> {
    // Almost identical to parse_infix_first(), but we do not accept unary ops.
    let (input, left) = parse_expr_nobin_noun(input)?;
    parse_infix_left(input, min_prec, left)
}
fn parse_infix_left<'a>(
    input_bin: Span<'a>,
    min_prec: u8,
    left: Expr<'a>,
) -> IResult<'a, Span<'a>, Expr<'a>> {
    // See if we can find an infix function. If not, return what we have.
    let (input, (span, func_map)) = match tag_infix(input_bin) {
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
    let (input, right) = parse_infix_prec(input, new_min_prec)?;

    // Put together the infix function with arguments. Continue forward.
    let expr = Expr {
        span,
        val: Value::Func(func_map.func, Box::new(left), Box::new(right)),
    };
    parse_infix_left(input, min_prec, expr)
}

fn tag_unary(input: Span) -> IResult<Span, SpanFuncmap> {
    tag_func(input, &UNARYS)
}

fn tag_infix(input: Span) -> IResult<Span, SpanFuncmap> {
    tag_func(input, &INFIXS)
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
        val: None,
        error: ErrorKind::Nom(error::ErrorKind::Tag),
    }))
}

#[derive(Debug)]
enum SimpleExpr<'a> {
    Int(&'a i32),
    UnSub(Box<SimpleExpr<'a>>),
    Pow(Box<SimpleExpr<'a>>, Box<SimpleExpr<'a>>),
    Mult(Box<SimpleExpr<'a>>, Box<SimpleExpr<'a>>),
    Div(Box<SimpleExpr<'a>>, Box<SimpleExpr<'a>>),
    Add(Box<SimpleExpr<'a>>, Box<SimpleExpr<'a>>),
    Sub(Box<SimpleExpr<'a>>, Box<SimpleExpr<'a>>),
}

impl<'a> SimpleExpr<'a> {
    fn new(tree: &'a Expr) -> Self {
        match tree {
            Expr {
                val: Value::Int(int),
                ..
            } => SimpleExpr::Int(&int),
            Expr {
                val: Value::UnFunc(Function::UnSub, x),
                ..
            } => SimpleExpr::UnSub(Box::new(SimpleExpr::new(x))),
            Expr {
                val: Value::Func(Function::Pow, x, y),
                ..
            } => SimpleExpr::Pow(Box::new(SimpleExpr::new(x)), Box::new(SimpleExpr::new(y))),
            Expr {
                val: Value::Func(Function::Mult, x, y),
                ..
            } => SimpleExpr::Mult(Box::new(SimpleExpr::new(x)), Box::new(SimpleExpr::new(y))),
            Expr {
                val: Value::Func(Function::Div, x, y),
                ..
            } => SimpleExpr::Div(Box::new(SimpleExpr::new(x)), Box::new(SimpleExpr::new(y))),
            Expr {
                val: Value::Func(Function::Add, x, y),
                ..
            } => SimpleExpr::Add(Box::new(SimpleExpr::new(x)), Box::new(SimpleExpr::new(y))),
            Expr {
                val: Value::Func(Function::Sub, x, y),
                ..
            } => SimpleExpr::Sub(Box::new(SimpleExpr::new(x)), Box::new(SimpleExpr::new(y))),
            _ => panic!(),
        }
    }
}

impl<'a> error::ParseError<Span<'a>> for Error<'a> {
    fn from_error_kind(input: Span<'a>, kind: error::ErrorKind) -> Self {
        Error {
            input,
            val: None,
            error: ErrorKind::Nom(kind),
        }
    }

    fn append(_: Span<'a>, _: error::ErrorKind, other: Self) -> Self {
        other
    }
}

struct Error<'a> {
    input: Span<'a>,
    val: Option<Span<'a>>,
    error: ErrorKind,
}

enum ErrorKind {
    NotRecognised,
    Incomplete,
    ParseInt(std::num::ParseIntError),
    Nom(error::ErrorKind),
}

impl ErrorKind {
    fn description(&self) -> String {
        match self {
            ErrorKind::NotRecognised => String::from("Failed to parse input."),
            ErrorKind::Incomplete => String::from("There was not enough data."),
            ErrorKind::ParseInt(err) => format!("Parse Int Error ({})", &err),
            ErrorKind::Nom(err) => String::from(err.description()),
        }
    }
}
