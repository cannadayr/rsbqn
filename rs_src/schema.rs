rustler::atoms!{ok}
use std::sync::Mutex;

pub type Id = u64;

struct Block {
    t:  Id,
    i:  Id,
    st: Id,
    l:  Id,
}

pub struct E {
    parent: Id,
    slots: Vec<Block>
}
impl E {
    pub fn new(parent: Id) -> Self {
        let slots = Vec::new();
        Self {
            parent: parent,
            slots: slots,
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
