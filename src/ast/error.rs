use super::Span;

pub trait Error {
    fn span(&self) -> Option<Span>;
    fn description(&self) -> String;
}
