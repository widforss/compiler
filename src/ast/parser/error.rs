use super::{Span, Error};
use nom::Err;

pub type IResult<'a, I, O, E = ParseError<'a>> = Result<(I, O), Err<E>>;

pub struct ParseError<'a> {
    pub input: Span<'a>,
    pub span: Option<Span<'a>>,
    pub error: ErrorKind,
}

pub enum ErrorKind {
    NotRecognised,
    ParseInt,
    ParseFloat,
    ParseType,
    ParseStatement,
    ParseFunction,
    DoubleFunctionDecl,
    Keyword,
    NoMain,
    MainReturns,
    Nom(nom::error::ErrorKind),
}

impl<'a> ParseError<'a> {
    pub fn new(input: Span<'a>, span: Option<Span<'a>>, error: ErrorKind) -> Self {
        ParseError { input, span, error }
    }
}

impl<'a> Error for ParseError<'a> {
    fn span(&self) -> Option<Span> {
        self.span
    }

    fn description(&self) -> String {
        use ErrorKind::*;

        let string = match self.error {
            NotRecognised => "Failed to parse input",
            ParseInt => "Failed to parse integer",
            ParseFloat => "Failed to parse float",
            ParseType => "Failed to parse type",
            ParseStatement => "Failed to parse statement",
            ParseFunction => "Failed to parse function",
            DoubleFunctionDecl => "Function was declared twice",
            Keyword => "Did not expect keyword",
            NoMain => "main function not declared.", 
            MainReturns => "main returns non-unit type",
            Nom(_) => panic!(),
        };
        String::from(string)
    }
}

impl<'a> nom::error::ParseError<Span<'a>> for ParseError<'a> {
    fn from_error_kind(input: Span<'a>, kind: nom::error::ErrorKind) -> Self {
        ParseError {
            input,
            span: None,
            error: ErrorKind::Nom(kind),
        }
    }

    fn append(_: Span<'a>, _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}
