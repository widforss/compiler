mod error;
mod expr;
mod util;

pub use error::{Error, ErrorKind};
use expr::{Expr, Function};
use nom::{
    branch, bytes::complete as bytes, character::complete as character, multi, sequence, Err,
};
use std::collections::HashMap;
use std::fmt;
use util::{IResult, Span};

pub struct Ast<'a>(HashMap<&'a str, Func<'a>>);

pub struct Func<'a> {
    typ: Type,
    params: Vec<(Type, &'a str)>,
    body: Stmt<'a>,
    span: Span<'a>,
}

pub struct Stmt<'a> {
    stmt: Statement<'a>,
    span: Span<'a>,
}

pub enum Statement<'a> {
    Let(bool, &'a str, Type, Expr<'a>),
    Assign(&'a str, Expr<'a>),
    While(Expr<'a>, Box<Stmt<'a>>),
    IfElse(Expr<'a>, Box<Stmt<'a>>, Box<Stmt<'a>>),
    Block(Vec<Stmt<'a>>),
    Return(Expr<'a>),
    Print(Expr<'a>),
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Bool,
    Int,
    Float,
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
            parse_type,
        ),
        parse_block,
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
                parse_type,
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
        Err(Err::Failure(Error {
            input,
            error: ErrorKind::Nom(_),
            ..
        })) => {
            return Err(Err::Failure(Error::new(
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

    let (input, _) = bytes::tag("let ")(input)?;
    let parser = sequence::tuple((
        sequence::preceded(
            character::multispace0,
            branch::alt((bytes::tag("mut"), bytes::tag(""))),
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
            sequence::preceded(bytes::tag(" "), Expr::parse),
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
            sequence::preceded(bytes::tag(" "), Expr::parse),
        )),
        parse_block,
        multi::many_m_n(
            0,
            1,
            sequence::preceded(
                sequence::tuple((character::multispace0, bytes::tag("else"))),
                branch::alt((
                    parse_block,
                    sequence::preceded(bytes::tag(" "), parse_if_else),
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
            sequence::preceded(bytes::tag(" "), Expr::parse),
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
            sequence::preceded(bytes::tag(" "), Expr::parse),
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
            stmt: Statement::Assign(ident.fragment, expr),
            span,
        },
    ))
}

fn parse_type(input: Span) -> IResult<Span, Type> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let (input, type_str) = util::parse_ident_span(input)?;
    let typ = match type_str.fragment {
        "i64" => Type::Int,
        "f64" => Type::Float,
        "bool" => Type::Bool,
        _ => {
            return Err(Err::Error(Error::new(
                orig_input,
                Some(type_str),
                ErrorKind::ParseType,
            )))
        }
    };
    Ok((input, typ))
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

impl<'a> fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.stmt)
    }
}

impl<'a> fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print(2))
    }
}

impl<'a> Statement<'a> {
    fn block(&self, indent: usize) -> String {
        let instr = "    ".repeat(indent);
        if let Statement::Block(stmts) = self {
            let mut string = String::from("Block [\n");
            for stmt in stmts.iter() {
                let statement = &stmt.stmt;
                string = format!("{}{}", string, statement.print(indent + 1));
            }
            format!("{}{}]", string, instr)
        } else {
            panic!();
        }
    }

    fn print(&self, indent: usize) -> String {
        let instr = "    ".repeat(indent);
        match self {
            Statement::Block(statements) => {
                let mut string = format!("{}Block [\n", instr);
                for stmt in statements.iter() {
                    let statement = &stmt.stmt;
                    string = format!("{}{}", string, statement.print(indent + 1));
                }
                format!("{}{}],\n", string, instr)
            }
            Statement::Let(mutable, ident, typ, expr) => format!(
                "{}Let: {{\n\
                 {}    Mutable: {:?},\n\
                 {}    Identifier: Ident({}),\n\
                 {}    Type: {:?},\n\
                 {}    Expression: {},\n\
                 {}}},\n",
                instr,
                instr,
                mutable,
                instr,
                ident,
                instr,
                typ,
                instr,
                expr.value.print(indent + 1),
                instr,
            ),
            Statement::While(expr, stmt) => {
                let statement = &stmt.stmt;
                format!(
                    "{}While: {{\n\
                     {}    Condition: {},\n\
                     {}    Do: {},\n\
                     {}}},\n",
                    instr,
                    instr,
                    expr.value.print(indent + 1),
                    instr,
                    statement.block(indent + 1),
                    instr,
                )
            }
            Statement::IfElse(expr, if_stmt, else_stmt) => {
                let if_stmt = &if_stmt.stmt;
                let else_stmt = &else_stmt.stmt;
                format!(
                    "{}IfElse: {{\n\
                     {}    Condition: {}\n\
                     {}    If: {},\n\
                     {}    Else: {},\n\
                     {}}},\n",
                    instr,
                    instr,
                    expr.value.print(indent + 1),
                    instr,
                    if_stmt.block(indent + 1),
                    instr,
                    else_stmt.block(indent + 1),
                    instr,
                )
            }
            Statement::Return(expr) => {
                format!("{}Return: {},\n", instr, expr.value.print(indent + 1))
            }
            Statement::Assign(ident, expr) => format!(
                "{}Assign: {{\n\
                 {}    Identifier: Ident({}),\n\
                 {}    Expression: {},\n\
                 {}}},\n",
                instr,
                instr,
                ident,
                instr,
                expr.value.print(indent + 1),
                instr
            ),
            Statement::Print(expr) => {
                format!("{}Print: {},\n", instr, expr.value.print(indent + 1))
            }
        }
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
