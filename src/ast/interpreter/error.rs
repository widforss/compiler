use super::super::Span;

pub struct Error<'a> {
    pub span: Option<Span<'a>>,
    pub error: ErrorKind,
}

pub enum ErrorKind {
    FuncNotFound,
    VarNotFound,
    ReturnType,
    ArgsNum,
    TypeError,
    NotMutable,
    DivisionByZero,
    Overflow,
}

impl<'a> Error<'a> {
    pub fn new(span: Option<Span<'a>>, error: ErrorKind) -> Self {
        Error { error, span }
    }
}

const ERR_MSG_FUNCNOTFOUND: &str = "Called non-existent function";
const ERR_MSG_VARNOTFOUND: &str = "Variable not declared";
const ERR_MSG_RETURNTYPE: &str = "Function return value of other type than declared";
const ERR_MSG_ARGSNUM: &str = "Number of arguments did not match parameters";
const ERR_MSG_TYPEERROR: &str = "Value had unexpected type";
const ERR_MSG_NOTMUTABLE: &str = "Tried to modify immutable variable";
const ERR_MSG_DIVISIONBYZERO: &str = "Division by zero";
const ERR_MSG_OVERFLOW: &str = "Arithmetic operation caused overflow";
impl ErrorKind {
    pub fn description(&self) -> &str {
        use ErrorKind::*;

        match self {
            FuncNotFound => ERR_MSG_FUNCNOTFOUND,
            VarNotFound => ERR_MSG_VARNOTFOUND,
            ReturnType => ERR_MSG_RETURNTYPE,
            ArgsNum => ERR_MSG_ARGSNUM,
            TypeError => ERR_MSG_TYPEERROR,
            NotMutable => ERR_MSG_NOTMUTABLE,
            DivisionByZero => ERR_MSG_DIVISIONBYZERO,
            Overflow => ERR_MSG_OVERFLOW,
        }
    }
}
