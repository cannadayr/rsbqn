rustler::atoms!{ok}

pub type Id = u64;

pub struct State {
    root: Id,
}
impl State {
    pub fn new(root: Id) -> Self {
        Self { root: root}
    }
}
