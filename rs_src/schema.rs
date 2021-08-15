use std::sync::Arc;
use std::sync::Mutex;
use once_cell::sync::OnceCell;
//use std::collections::HashMap; // TODO

rustler::atoms!{ok}

#[derive(Debug)]
pub enum V {
    BlockInst,
}
pub type Vn = Option<V>;
#[derive(Default, Debug)]
pub struct Code<'a> {
    bc:    Vec<usize>,
    objs:  Vec<V>,
    pub blocks:LateInit<&'a Vec<Arc<Block<'a>>>>,
}
#[derive(Default, Debug)]
pub struct Block<'a> {
    pub typ:u8, pub imm:bool, pub locals:usize, pub pos:usize,
    pub code:LateInit<&'a Arc<Code<'a>>>,
}
struct Env {
    parent: Arc<Env>,
    vars:   Vec<Vn>,
}
struct BlockInst<'a> {
    typ:   u8,
    def:   Arc<&'a Block<'a>>,
    parent:Env,
    args:  Vec<Vn>,
}
pub struct State {
    root: usize,
    pos:  usize,
    heap: Vec<Option<Env>>,
}
impl State {
    pub fn new() -> Self {
        Self { root: 0, pos: 0, heap: Vec::new(), }
    }
}
pub struct Container {
    pub mutex: Mutex<State>,
}

// https://github.com/rust-lang/rfcs/pull/2788
#[derive(Debug)]
pub struct LateInit<T> {
    cell: OnceCell<T>,
}

impl<T> LateInit<T> {
    pub fn init(&self, value: T) {
        assert!(self.cell.set(value).is_ok())
    }
}

impl<T> Default for LateInit<T> {
    fn default() -> Self { LateInit { cell: OnceCell::default() } }
}

impl<T> std::ops::Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.cell.get().unwrap()
    }
}
