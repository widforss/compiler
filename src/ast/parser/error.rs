use super::super::Span;
use nom::Err;

pub type IResult<'a, I, O, E = Error<'a>> = Result<(I, O), Err<E>>;

pub struct Error<'a> {
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

impl<'a> Error<'a> {
    pub fn new(input: Span<'a>, span: Option<Span<'a>>, error: ErrorKind) -> Self {
        Error { input, span, error }
    }
}

const ERR_MSG_NOTRECOGNISED: &str = "Failed to parse input";
const ERR_MSG_PARSEINT: &str = "Failed to parse integer";
const ERR_MSG_PARSEFLOAT: &str = "Failed to parse float";
const ERR_MSG_PARSETYPE: &str = "Failed to parse type";
const ERR_MSG_PARSESTMT: &str = "Failed to parse statement";
const ERR_MSG_PARSEFUNC: &str = "Failed to parse function";
const ERR_MSG_DOUBLEFUNC: &str = "Function was declared twice";
const ERR_MSG_KEYWORD: &str = "Did not expect keyword";
const ERR_MSG_NOMAIN: &str = "main function not declared.";
const ERR_MSG_MAINRETURNS: &str = "main returns non-unit type";
impl ErrorKind {
    pub fn description(&self) -> &str {
        use ErrorKind::*;

        match self {
            NotRecognised => ERR_MSG_NOTRECOGNISED,
            ParseInt => ERR_MSG_PARSEINT,
            ParseFloat => ERR_MSG_PARSEFLOAT,
            ParseType => ERR_MSG_PARSETYPE,
            ParseStatement => ERR_MSG_PARSESTMT,
            ParseFunction => ERR_MSG_PARSEFUNC,
            DoubleFunctionDecl => ERR_MSG_DOUBLEFUNC,
            Keyword => ERR_MSG_KEYWORD,
            NoMain => ERR_MSG_NOMAIN,
            MainReturns => ERR_MSG_MAINRETURNS,
            Nom(err) => err.description(),
        }
    }
}

impl<'a> nom::error::ParseError<Span<'a>> for Error<'a> {
    fn from_error_kind(input: Span<'a>, kind: nom::error::ErrorKind) -> Self {
        Error {
            input,
            span: None,
            error: ErrorKind::Nom(kind),
        }
    }

    fn append(_: Span<'a>, _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}
