use super::Type;

#[derive(Copy, Clone)]
pub struct TypeVariable<'a> {
    pub typ: &'a Type,
    pub mutable: bool,
}
