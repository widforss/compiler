use super::util::Span;

#[derive(Debug)]
pub struct Error<'a> {
    pub input: Span<'a>,
    pub span: Option<Span<'a>>,
    pub error: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    NotRecognised,
    ParseInt,
    ParseFloat,
    ParseType,
    ParseStatement,
    ParseFunction,
    DoubleFunctionDecl,
    Keyword,
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
impl ErrorKind {
    pub fn description(&self) -> &str {
        match self {
            ErrorKind::NotRecognised => ERR_MSG_NOTRECOGNISED,
            ErrorKind::ParseInt => ERR_MSG_PARSEINT,
            ErrorKind::ParseFloat => ERR_MSG_PARSEFLOAT,
            ErrorKind::ParseType => ERR_MSG_PARSETYPE,
            ErrorKind::ParseStatement => ERR_MSG_PARSESTMT,
            ErrorKind::ParseFunction => ERR_MSG_PARSEFUNC,
            ErrorKind::DoubleFunctionDecl => ERR_MSG_DOUBLEFUNC,
            ErrorKind::Keyword => ERR_MSG_KEYWORD,
            ErrorKind::Nom(err) => err.description(),
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
