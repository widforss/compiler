use super::Span;
use std::collections::HashMap;

pub struct State<T> {
    state: HashMap<String, Vec<(T, u64)>>,
    scope: u64,
    depth: u64,
    curvars: Vec<Vec<String>>,
}

impl<T> State<T> {
    pub fn new(depth: u64) -> Self {
        State {
            state: HashMap::new(),
            scope: 0,
            depth: depth,
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

    pub fn get(&self, span: Span) -> Option<&T> {
        let ident = String::from(span.fragment);
        if let Some(vector) = self.state.get(&ident) {
            Some(&vector.last().unwrap().0)
        } else {
            None
        }
    }

    pub fn set(&mut self, span: Span, value: T) {
        let ident = String::from(span.fragment);
        let vector = self.state.get_mut(&ident).unwrap();
        let (_, scope) = vector.pop().unwrap();
        vector.push((value, scope));
    }

    pub fn insert(&mut self, span: Span, var: T) {
        let ident = String::from(span.fragment);
        if let Some(vector) = self.state.get_mut(&ident) {
            vector.push((var, self.scope));
        } else {
            self.state.insert(ident.clone(), vec![(var, self.scope)]);
            self.curvars.last_mut().unwrap().push(ident.clone());
        };
    }

    pub fn depth(&self) -> u64 {
        self.depth
    }
}
