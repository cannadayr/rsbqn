use std::sync::Arc;
use std::sync::Mutex;
use once_cell::sync::OnceCell;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};
use rustler::{Encoder};

rustler::atoms!{ok}

#[derive(Debug,Clone)]
pub enum V {
    Scalar(f64),
    BlockInst,
}
impl Trace for V {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing V");
    }
}
impl Trace for &V {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing &V");
    }
}
impl Encoder for V {
    fn encode<'a>(&self, env: rustler::Env<'a>) -> rustler::Term<'a> {
        match self {
            V::Scalar(n) => n.encode(env),
            V::BlockInst => panic!("can't encode blockinst to BEAM"),
        }
    }
}
#[derive(Debug,Clone)]
pub enum Vs {
    Ref(Cc<V>),
    Slot(EnvRef,usize),
}
impl Vs {
    pub fn set(&self,d: bool,vs: Vs) -> Cc<V> {
        match (self,vs) {
            (Vs::Slot(env,id),Vs::Ref(v)) => { env.set(*id,v) },
            _ => panic!("can only set slots"),
        }
    }
}
impl Encoder for Vs {
    fn encode<'a>(&self, env: rustler::Env<'a>) -> rustler::Term<'a> {
        match self {
            Vs::Ref(r) => (**r).encode(env),
            Vs::Slot(env,slot) => panic!("cant encode slot to BEAM"),
        }
    }
}
pub type Vn = Option<V>;

#[derive(Debug)]
pub enum Vu {
    Undefined,
    None,
    V(Cc<V>),
}

#[derive(Default,Debug)]
pub struct Code<'a> {
    pub bc:    Vec<usize>,
    pub objs:  Vec<Cc<V>>,
    pub blocks:LateInit<Vec<Arc<Block<'a>>>>,
}
impl<'a> Code<'a> {
    pub fn new(bc: Vec<usize>,objs: Vec<Cc<V>>,blocks_raw: Vec<(u8,bool,usize,usize)>) -> Arc<Self> {
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

#[derive(Clone,Default,Debug)]
pub struct EnvRef(Cc<Mutex<Env>>);
impl EnvRef {
    pub fn new(env: Env) -> Self {
        EnvRef(Cc::new(Mutex::new(env)))
    }
    pub fn set(&self,id: usize,v: Cc<V>) -> Cc<V> {
        match self {
            EnvRef(arc) => {
                let mut guard = arc.lock().unwrap();
                (*guard).vars[id] = Vu::V(v.clone());
                v
            },
        }
    }
}
#[derive(Default,Debug)]
pub struct Env {
    pub parent:Option<EnvRef>,
    pub vars:   Vec<Vu>,
}
impl Trace for Env {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing env");
    }
}

struct BlockInst<'a> {
    typ:   u8,
    def:   Arc<&'a Block<'a>>,
    parent:EnvRef,
    args:  Vec<Vn>,
}

#[derive(Default,Debug)]
pub struct State {
    pub root: EnvRef,
}
impl State {
    pub fn new(block: &Arc<Block>) -> Self {
        debug!("block {}",block.locals);
        let mut vars: Vec<Vu> = Vec::with_capacity(block.locals);
        vars.resize_with(block.locals, || Vu::None);
        let env = Env {parent: None, vars: vars};
        Self {root: EnvRef::new(env) }
    }
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
