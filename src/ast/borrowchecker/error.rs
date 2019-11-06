use super::{Error, Span};

pub struct BorrowError<'a> {
    pub span: Option<Span<'a>>,
    pub error: ErrorKind,
}

pub enum ErrorKind {
    ReturnedLifetime,
    LifetimeError,
    UndeclaredLifetime,
    SameReference,
}

impl<'a> BorrowError<'a> {
    pub fn new(span: Option<Span<'a>>, error: ErrorKind) -> Self {
        BorrowError { error, span }
    }
}

impl<'a> Error for BorrowError<'a> {
    fn span(&self) -> Option<Span> {
        self.span
    }

    fn description(&self) -> String {
        use ErrorKind::*;

        let string = match self.error {
            ReturnedLifetime => "Lifetime does not match return type",
            LifetimeError => "Lifetime is ambiguous",
            UndeclaredLifetime => "Undeclared lifetime",
            SameReference => "Reference root reuse",
        };
        String::from(string)
    }
}
