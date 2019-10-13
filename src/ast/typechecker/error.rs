use super::{Span, Error};

pub struct TypeError<'a> {
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
}

impl<'a> TypeError<'a> {
    pub fn new(span: Option<Span<'a>>, error: ErrorKind) -> Self {
        TypeError { error, span }
    }
}

impl<'a> Error for TypeError<'a> {
    fn span(&self) -> Option<Span> {
        self.span
    }

    fn description(&self) -> String {
        use ErrorKind::*;

        let string = match self.error {
            FuncNotFound => "Called non-existent function",
            VarNotFound => "Variable not declared",
            ReturnType => "Function does not return value",
            ArgsNum => "Number of arguments did not match parameters",
            TypeError => "Value had unexpected type",
            NotMutable => "Tried to modify immutable variable",
        };
        String::from(string)
    }
}
