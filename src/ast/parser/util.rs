use super::{ErrorKind, IResult, ParseError, Span};
use nom::{
    branch, bytes::complete as bytes, character::complete as character, multi, sequence, Err,
};

const KEYWORDS: [&str; 12] = [
    "fn", "let", "mut", "while", "if", "else", "return", "print", "INFINITY", "true", "false", "_",
];

pub fn short<I, O>(tuple: IResult<I, O>) -> IResult<I, O> {
    match tuple {
        Ok(res) => Ok(res),
        Err(Err::Error(err)) => Err(Err::Failure(err)),
        Err(Err::Failure(err)) => Err(Err::Failure(err)),
        Err(Err::Incomplete(_)) => panic!(),
    }
}

pub fn parse_ident_span(input: Span) -> IResult<Span, Span> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;
    let mut span = input;

    let parser1 = sequence::tuple((
        character::alpha1,
        multi::many0(branch::alt((
            character::alpha1,
            character::digit1,
            bytes::tag("_"),
        ))),
    ));
    let parser2 = sequence::tuple((
        bytes::tag("_"),
        multi::many1(branch::alt((
            character::alpha1,
            character::digit1,
            bytes::tag("_"),
        ))),
    ));
    let parser = sequence::tuple((branch::alt((parser1, parser2)), bytes::tag("")));

    let (input, (_, end)) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    for keyword in KEYWORDS.iter() {
        if span.fragment == *keyword {
            return Err(Err::Error(ParseError::new(
                orig_input,
                Some(span),
                ErrorKind::Keyword,
            )));
        }
    }
    Ok((input, span))
}

pub fn parse_lifetime(input: Span) -> IResult<Span, Span> {
    let (input, _) = character::multispace0(input)?;
    let orig_input = input;

    let parser = sequence::tuple((
        sequence::preceded(
            sequence::tuple((character::multispace0, bytes::tag("'"))),
            parse_ident_span,
        ),
        bytes::tag(""),
    ));

    let (input, (mut life, end)) = parser(input)?;

    let begin = life.offset - orig_input.offset - 1;
    let end = end.offset - orig_input.offset;
    life.fragment = &orig_input.fragment[begin..end];
    life.offset -= 1;

    Ok((input, life))
}
