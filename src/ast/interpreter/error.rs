use super::{Error, Span};

pub struct IntprtError<'a> {
    pub span: Option<Span<'a>>,
    pub error: ErrorKind,
}

pub enum ErrorKind {
    DivisionByZero,
    Overflow,
    StackDepth,
}

impl<'a> IntprtError<'a> {
    pub fn new(span: Option<Span<'a>>, error: ErrorKind) -> Self {
        IntprtError { error, span }
    }
}

impl<'a> Error for IntprtError<'a> {
    fn span(&self) -> Option<Span> {
        self.span
    }

    fn description(&self) -> String {
        use ErrorKind::*;

        let string = match self.error {
            DivisionByZero => "Division by zero",
            Overflow => "Arithmetic operation caused overflow",
            StackDepth => "Maximum stack depth reached",
        };
        String::from(string)
    }
}
