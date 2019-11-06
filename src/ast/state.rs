use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct State<T>(Vec<LocalState<T>>);

#[derive(Clone, Debug)]
struct LocalState<T> {
    state: HashMap<String, Vec<(T, u64)>>,
    anonymous: Vec<(T, u64)>,
    scope: u64,
    curvars: Vec<Vec<String>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Pointer<'a> {
    localpointer: LocalPointer<'a>,
    depth: usize,
}

#[derive(Clone, Copy, Debug)]
struct LocalPointer<'a> {
    ident: Option<&'a str>,
    index: usize,
}

impl<T> State<T> {
    pub fn new() -> Self {
        State(vec![LocalState::new()])
    }

    pub fn add(&mut self) {
        let State(localstate) = self;
        localstate.push(LocalState::new());
    }

    pub fn rem(&mut self) {
        let State(localstate) = self;
        localstate.pop();
    }

    pub fn inc(&mut self) {
        let State(localstate) = self;
        localstate.last_mut().unwrap().inc()
    }

    pub fn dec(&mut self) {
        let State(localstate) = self;
        localstate.last_mut().unwrap().dec()
    }

    pub fn get(&mut self, ident: &str) -> Option<&T> {
        let State(localstate) = self;
        localstate.last_mut().unwrap().get(ident)
    }

    pub fn get_mut(&mut self, ident: &str) -> Option<&mut T> {
        let State(localstate) = self;
        localstate.last_mut().unwrap().get_mut(ident)
    }

    pub fn insert(&mut self, ident: &str, var: T) {
        let State(localstate) = self;
        localstate.last_mut().unwrap().insert(ident, var)
    }

    pub fn ref_var<'b>(&self, ident: &'b str) -> Option<Pointer<'b>> {
        let State(localstate) = self;
        for state in localstate.iter().rev() {
            if let Some(localpointer) = state.ref_var(ident) {
                let pointer = Pointer {
                    localpointer,
                    depth: localstate.len() - 1,
                };
                return Some(pointer);
            }
        }
        None
    }

    pub fn ref_literal<'b>(&mut self, literal: T) -> Pointer<'b> {
        let State(localstate) = self;
        let localpointer = localstate.last_mut().unwrap().ref_literal(literal);
        Pointer {
            localpointer,
            depth: localstate.len() - 1,
        }
    }

    pub fn deref(&mut self, pointer: Pointer) -> &mut T {
        let State(localstate) = self;
        let Pointer { localpointer, .. } = pointer;
        let state = localstate.get_mut(pointer.depth).unwrap();
        state.deref(localpointer)
    }

    pub fn depth(&self) -> usize {
        let State(localstate) = self;
        localstate.len() - 1
    }
}

impl<T> LocalState<T> {
    pub fn new() -> Self {
        LocalState {
            state: HashMap::new(),
            anonymous: vec![],
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
        while let Some((_, scope)) = self.anonymous.last() {
            if *scope < self.scope {
                break;
            }
            self.anonymous.pop();
        }
        self.scope -= 1;
        self.curvars.pop();
    }

    pub fn get(&mut self, ident: &str) -> Option<&T> {
        let res = self.get_mut(ident)?;
        Some(&*res)
    }

    pub fn get_mut(&mut self, ident: &str) -> Option<&mut T> {
        let ident = String::from(ident);
        if let Some(vector) = self.state.get_mut(&ident) {
            Some(&mut vector.last_mut().unwrap().0)
        } else {
            None
        }
    }

    pub fn insert(&mut self, ident: &str, var: T) {
        let ident = String::from(ident);
        if let Some(vector) = self.state.get_mut(&ident) {
            vector.push((var, self.scope));
        } else {
            self.state.insert(ident.clone(), vec![(var, self.scope)]);
            self.curvars.last_mut().unwrap().push(ident.clone());
        };
    }

    pub fn ref_var<'b>(&self, ident: &'b str) -> Option<LocalPointer<'b>> {
        let string = String::from(ident);
        if let Some(vector) = self.state.get(&string) {
            Some(LocalPointer {
                ident: Some(ident),
                index: vector.len() - 1,
            })
        } else {
            None
        }
    }

    pub fn ref_literal<'b>(&mut self, literal: T) -> LocalPointer<'b> {
        self.anonymous.push((literal, self.scope));
        LocalPointer {
            ident: None,
            index: self.anonymous.len() - 1,
        }
    }

    pub fn deref(&mut self, localpointer: LocalPointer) -> &mut T {
        match localpointer.ident {
            Some(ident) => {
                &mut self
                    .state
                    .get_mut(ident)
                    .unwrap()
                    .get_mut(localpointer.index)
                    .unwrap()
                    .0
            }
            None => &mut self.anonymous.get_mut(localpointer.index).unwrap().0,
        }
    }
}

impl<'a> PartialEq for Pointer<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.localpointer == other.localpointer && self.depth == other.depth
    }
}

impl<'a> PartialEq for LocalPointer<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.index == other.index
    }
}
