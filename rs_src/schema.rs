use std::sync::Arc;
use std::sync::Mutex;
use once_cell::sync::OnceCell;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};

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
    pub blocks:LateInit<Vec<Arc<Block<'a>>>>,
}
impl<'a> Code<'a> {
    pub fn new(bc: Vec<usize>,objs: Vec<V>,blocks_raw: Vec<(u8,bool,usize,usize)>) -> Arc<Self> {
        let code = Arc::new(Self {bc: bc, objs: objs, ..Code::default()});
        let blocks_derv = blocks_raw.iter().map(|block|
            match block {
                (typ,imm,locals,pos) => {
                    let b = Block { typ: *typ, imm: *imm, locals: *locals, pos: *pos, .. Block::default() };
                    b.code.init(code.clone());
                    Arc::new(b)
                }
            }
        ).collect::<Vec<Arc<Block>>>();
        code.blocks.init(blocks_derv);
        code
    }
}
#[derive(Default, Debug)]
pub struct Block<'a> {
    pub typ:u8, pub imm:bool, pub locals:usize, pub pos:usize,
    pub code:LateInit<Arc<Code<'a>>>,
}
#[derive(Default,Debug)]
pub struct Env<'a> {
    pub parent:LateInit<&'a Arc<Env<'a>>>,
    pub vars:   Vec<Vn>,
}
impl<'a> Trace for Env<'a> {
    fn trace(&self, tracer: &mut Tracer) {
        println!("tracing!");
    }

}

struct BlockInst<'a> {
    typ:   u8,
    def:   Arc<&'a Block<'a>>,
    parent:Env<'a>,
    args:  Vec<Vn>,
}
#[derive(Default,Debug)]
pub struct State {
    root: Cc<Mutex<Env<'static>>>,
}
impl State {
    pub fn new() -> Self {
        Self {root: Cc::new(Mutex::new(Env{vars: Vec::new(), ..Env::default()}))}
    }
    /*
    pub fn alloc(&mut self,env: Cc<Mutex<Env>>) {
        self.heap.push(env);
    }
    */
}
/*
pub struct Container<'a> {
    pub mutex: Mutex<State<'a>>,
}
*/

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
