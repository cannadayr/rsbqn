rustler::atoms!{ok}
use std::sync::Mutex;
use std::convert::TryFrom;

pub type Id = usize;

pub trait ToEntity {
    fn to_entity(&self) -> Entity;
}
pub trait FromEntity {
    fn from_entity(&self) -> Entity;
}
#[derive(Debug,Clone,Copy)]
pub enum Entity {
    Id(Id),
}
impl ToEntity for usize {
    fn to_entity(&self) -> Entity {
        Entity::Id(*self)
    }
}

pub struct A {
    pub sh: Vec<Entity>,
    pub r: Vec<Entity>,
}

pub struct Prog {
    pub b: Vec<usize>,
    pub o: Vec<Id>,
    pub s: Vec<Block>,
}

#[derive(Clone,Copy)]
pub struct Block {
    pub t:  Id,
    pub i:  Id,
    pub st: usize,
    pub l:  usize,
}

impl Block {
    pub fn new(bl: Vec<Id>) -> Self {
        let st =
            match usize::try_from(bl[2]) {
                Ok(b) => b,
                Err(e) => panic!("error loading block"),
            };
        let l =
            match usize::try_from(bl[3]) {
                Ok(b) => b,
                Err(e) => panic!("error loading block"),
            };
        Self {
            t: bl[0],
            i: bl[1],
            st: st,
            l: l,
        }
    }

}

pub struct Env {
    parent: Id,
    slots: Vec<Entity>
}
impl Env {
    pub fn new(parent: Id,capacity: usize) -> Self {
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
        let root = Env::new(root_id,0);
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
