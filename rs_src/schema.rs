rustler::atoms!{ok}
use std::sync::Mutex;

pub type Id = u64;

pub struct State {
    root: Id,
    id: Id,
}
impl State {
    pub fn new() -> Self {
        Self {
            root: 0,
            id: 0
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
