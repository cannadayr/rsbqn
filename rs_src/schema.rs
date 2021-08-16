use std::sync::Arc;
use std::sync::Mutex;
use once_cell::sync::OnceCell;

rustler::atoms!{ok}

#[derive(Debug)]
pub enum V {
    Float(f64),
    BlockInst,
}
pub type Vn = Option<V>;
#[derive(Default,Debug)]
pub struct Code<'a> {
    pub bc:    Vec<usize>,
    pub objs:  Vec<V>,
    pub blocks:LateInit<&'a Vec<Arc<Block<'a>>>>,
}
#[derive(Default, Debug)]
pub struct Block<'a> {
    pub typ:u8, pub imm:bool, pub locals:usize, pub pos:usize,
    pub code:LateInit<&'a Arc<Code<'a>>>,
}
#[derive(Default,Debug)]
pub struct Env<'a> {
    pub parent:LateInit<&'a Arc<Env<'a>>>,
    pub vars:   Vec<Vn>,
}
struct BlockInst<'a> {
    typ:   u8,
    def:   Arc<&'a Block<'a>>,
    parent:Env<'a>,
    args:  Vec<Vn>,
}
pub struct State<'a> {
    root: usize,
    pos:  usize,
    heap: Vec<Arc<Env<'a>>>,
}
impl<'a> State<'a> {
    pub fn new() -> Self {
        Self { root: 0, pos: 0, heap: Vec::new(), }
    }
    pub fn alloc(&mut self,env: Arc<Env<'a>>) -> usize {
        self.heap.push(env);
        self.pos += 1;
        self.pos - 1
    }
}
pub struct Container<'a> {
    pub mutex: Mutex<State<'a>>,
}

// https://docs.rs/once_cell/1.8.0/once_cell/#lateinit
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
