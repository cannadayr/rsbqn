rustler::atoms!{ok}
use std::sync::Mutex;
use std::convert::TryFrom;

pub type Id = u64;

#[derive(Clone,Copy)]
pub struct Block {
    pub t:  Id,
    pub i:  Id,
    pub st: Id,
    pub l:  usize,
}
impl Block {
    pub fn new(bl: Vec<Id>) -> Self {
        let l =
            match usize::try_from(bl[3]) {
                Ok(b) => b,
                Err(e) => panic!("error loading block"),
            };
        Self {
            t: bl[0],
            i: bl[1],
            st: bl[2],
            l: l,
        }
    }

}

pub struct Env {
    parent: Id,
    slots: Vec<Block>
}
impl Env {
    pub fn new(parent: Id) -> Self {
        let slots = Vec::new();
        Self {
            parent: parent,
            slots: slots,
        }
    }
    pub fn alloc(parent: Id,capacity: usize) -> Self {
        let slots = Vec::with_capacity(capacity);
        Self {
            parent: parent,
            slots: slots,
        }
    }
}

pub struct State {
    root: Id,
    id: Id,
    heap: Vec<Option<Env>>,
}
impl State {
    pub fn new() -> Self {
        let root_id = 0;
        let root = Env::new(root_id);
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
    pub fn root(&self) -> Id {
        self.root
    }
    pub fn incr(&mut self) {
        self.id += 1;
    }
    pub fn alloc(&mut self,e: Env) -> Id {
        self.heap.push(Some(e));
        self.id += 1;
        self.id
    }
}

pub struct Container {
    pub mutex: Mutex<State>,
}
