use rustler::{NifStruct};

rustler::atoms!{ok}

pub type Id = u64;

#[derive(NifStruct)]
#[module = "ebqn"]
pub struct State {
    pub root: Id
}
