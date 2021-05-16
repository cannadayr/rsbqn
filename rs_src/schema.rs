rustler::atoms!{ok}
use std::sync::Mutex;

pub type Id = u64;

pub struct E {
    parent: Id
}
impl E {
    pub fn new(parent: Id) -> Self {
        Self {
            parent: parent
        }
    }
}

pub struct State {
    root: Id,
    id: Id,
    heap: E,
}
impl State {
    pub fn new() -> Self {
        let root = 0;
        let e = E::new(root);
        Self {
            root: root,
            id: 1+root,
            heap: e,
        }
    }
    pub fn id(&self) -> Id {
        self.id
    }
    pub fn incr(&mut self) {
        self.id += 1;
    }
}

pub struct Container {
    pub mutex: Mutex<State>,
}
