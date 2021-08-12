use rustler::{Encoder};
use std::sync::Arc;
use std::sync::Mutex;

rustler::atoms!{ok}

pub enum V {
    BlockInst { typ: u8, def: Arc<Block>, parent: Env, args: Vec<Vn> },
}
pub type Vn = Option<V>;
struct Code {
    bc:    Vec<usize>,
    objs:  Vec<V>,
    blocks:Vec<Arc<Block>>,
}
struct Block {
    typ:u8, imm:bool, locals:usize,
    code: Arc<Code>, pos: usize,
}
struct Env {
    parent: Arc<Env>,
    vars:   Vec<Vn>,
}
struct BlockInst {
    typ:   u8,
    def:   Arc<Block>,
    parent:Env,
    args:  Vec<Vn>,
}
pub struct State {
    //root: usize,
    //pos:  usize,
    //heap: Vec<Option<Env>>,
}
pub struct Container {
    pub mutex: Mutex<State>,
}
