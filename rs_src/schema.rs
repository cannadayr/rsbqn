rustler::atoms!{ok}
use std::sync::Mutex;
use std::convert::TryFrom;
use rustler::{Encoder};

pub type Id = usize;

#[derive(Debug,Clone,Copy)]
pub enum Entity {
    Id(Id),
    Slot(Id,Id),
}
pub trait ToEntity {
    fn to_entity(&self) -> Entity;
}
impl ToEntity for usize {
    fn to_entity(&self) -> Entity {
        Entity::Id(*self)
    }
}
impl ToEntity for Slot {
    fn to_entity(&self) -> Entity {
        Entity::Slot((*self).0,(*self).1)
    }
}

impl<'a> Encoder for Entity {
    fn encode<'b>(&self, env: rustler::Env<'b>) -> rustler::Term<'b> {
        match self {
            Entity::Id(id) => id.encode(env),
            Entity::Slot(x,w) => (-1).encode(env), // don't expose to erlang
        }
    }
}

pub struct Slot (pub Id,pub Id);

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
    slots: Vec<Option<Entity>>
}
impl Env {
    pub fn new(parent: Id,capacity: usize) -> Self {
        let mut slots: Vec<Option<Entity>> = Vec::with_capacity(capacity);
        slots.resize_with(capacity, || None);
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
        self.id - 1
    }
    pub fn get(&self,eid: Id,sid: Id) -> Entity {
        let env =
            match &self.heap[eid] {
                Some(env) => env,
                _ => panic!("bad env"),
            };
        match env.slots[sid] {
            Some(entity) => entity,
            _ => panic!("bad entity"),
        }
    }
    pub fn set(&mut self,d: Id,a: Id,i: Id,v: Entity) {
        let mut env =
            match &mut self.heap[a] {
                Some(env) => env,
                _ => panic!("bad env"),
            };
        env.slots[i] = Some(v); // should we clone?
    }
}

pub struct Container {
    pub mutex: Mutex<State>,
}
