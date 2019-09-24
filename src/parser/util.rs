use super::error::{Error, ErrorKind};
use nom::{branch, bytes::complete::tag, character::complete as character, multi, sequence, Err};
use nom_locate::LocatedSpan;

const KEYWORDS: [&str; 9] = [
    "fn", "let", "mut", "while", "if", "else", "return", "print", "INFINITY",
];

pub type Span<'a> = LocatedSpan<&'a str>;
pub type IResult<'a, I, O, E = Error<'a>> = Result<(I, O), Err<E>>;

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
            tag("_"),
        ))),
    ));
    let parser2 = sequence::tuple((
        tag("_"),
        multi::many1(branch::alt((
            character::alpha1,
            character::digit1,
            tag("_"),
        ))),
    ));
    let parser = sequence::tuple((branch::alt((parser1, parser2)), tag("")));

    let (input, (_, end)) = parser(input)?;
    span.fragment = &span.fragment[..(end.offset - span.offset)];

    for keyword in KEYWORDS.iter() {
        if span.fragment == *keyword {
            return Err(Err::Error(Error::new(
                orig_input,
                Some(span),
                ErrorKind::Keyword,
            )));
        }
    }
    Ok((input, span))
}
