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
    heap: Vec<Option<E>>,
}
impl State {
    pub fn new() -> Self {
        let root_id = 0;
        let root = E::new(root_id);
        let mut heap = Vec::new();
        heap.push(Some(root));
        Self {
            root: root_id,
            id: 1+root_id,
            heap: heap,
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
