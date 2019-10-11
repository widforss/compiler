use super::error::{Error, ErrorKind};
use super::util;
use super::{Literal, Mutability, Span, Type};
use std::collections::HashMap;

#[derive(Debug)]
pub struct State {
    pub state: HashMap<String, Vec<(Variable, Scope)>>,
    pub scope: Scope,
    pub curvars: Vec<Vec<String>>,
}

#[derive(Debug)]
pub struct Variable {
    pub typ: Type,
    pub mutable: Mutability,
    pub value: Literal,
}

type Scope = u32;

impl State {
    pub fn new() -> Self {
        State {
            state: HashMap::new(),
            scope: 0,
            curvars: vec![vec![]],
        }
    }

    pub fn inc(&mut self) {
        self.scope += 1;
        self.curvars.push(vec![]);
    }

    pub fn dec(&mut self) {
        if self.scope == 0 {
            panic!();
        }

        let curvars = self.curvars.last_mut().unwrap();
        while let Some(ident) = curvars.pop() {
            let vector = self.state.get_mut(&ident).unwrap();
            while let Some((_, scope)) = vector.last() {
                if *scope < self.scope {
                    break;
                }
                vector.pop();
            }
            if vector.len() == 0 {
                self.state.remove(&ident);
            }
        }
        self.scope -= 1;
        self.curvars.pop();
    }

    pub fn get<'a, 'b>(&'a self, span: Span<'b>) -> Result<&'a Variable, Error<'b>> {
        let ident = String::from(span.fragment);
        match self.state.get(&ident) {
            Some(vector) => Ok(&vector.last().unwrap().0),
            None => return Err(Error::new(Some(span), ErrorKind::VarNotFound)),
        }
    }

    pub fn set<'b>(&mut self, span: Span<'b>, val: Literal) -> Result<(), Error<'b>> {
        let ident = String::from(span.fragment);
        match self.state.get_mut(&ident).unwrap().last_mut() {
            Some((var, _)) => {
                if !var.mutable {
                    return Err(Error::new(Some(span), ErrorKind::NotMutable));
                }
                var.value = util::check_type(var.typ, val, span)?
            }
            None => return Err(Error::new(Some(span), ErrorKind::VarNotFound)),
        };
        Ok(())
    }

    pub fn insert<'a>(&mut self, span: Span<'a>, var: Variable) {
        let ident = String::from(span.fragment);
        match self.state.get_mut(&ident) {
            Some(vector) => vector.push((var, self.scope)),
            None => {
                self.state.insert(ident.clone(), vec![(var, self.scope)]);
                self.curvars.last_mut().unwrap().push(ident.clone());
            }
        };
    }
}
