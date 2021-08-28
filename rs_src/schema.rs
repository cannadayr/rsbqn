use std::sync::Arc;
use std::sync::Mutex;
use once_cell::sync::OnceCell;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};
use rustler::{Encoder};

rustler::atoms!{ok}

// Value (unboxed)
#[derive(Debug,Clone)]
pub enum Vu {
    Scalar(f64),
    BlockInst,
}
impl Trace for Vu {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing V");
    }
}
impl Trace for &Vu {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing &V");
    }
}
impl Encoder for Vu {
    fn encode<'a>(&self, env: rustler::Env<'a>) -> rustler::Term<'a> {
        match self {
            Vu::Scalar(n) => n.encode(env),
            Vu::BlockInst => panic!("can't encode blockinst to BEAM"),
        }
    }
}

// Value
pub type V = Cc<Vu>;

// Value (Optional)
pub type Vn = Option<V>;

// Value (boxed on the stack)
#[derive(Debug,Clone)]
pub enum Vs {
    Ref(V),
    Slot(Env,usize),
}
impl Vs {
    pub fn to_ref(&self) -> &V {
        match self {
            Vs::Ref(v) => v,
            _ => panic!("can't convert to ref"),
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

// Value (boxed on the heap)
#[derive(Debug)]
pub enum Vh {
    Undefined,
    None,
    V(V),
}

// Code
#[derive(Default,Debug)]
pub struct Code {
    pub bc:    Vec<usize>,
    pub objs:  Vec<V>,
    pub blocks:LateInit<Vec<Cc<Block>>>,
}
impl Code {
    pub fn new(bc: Vec<usize>,objs: Vec<V>,blocks_raw: Vec<(u8,bool,usize,usize)>) -> Cc<Self> {
        let code = Cc::new(Self {bc: bc, objs: objs, ..Code::default()});
        let blocks_derv = blocks_raw.iter().map(|block|
            match block {
                (typ,imm,locals,pos) => {
                    let b = Block { typ: *typ, imm: *imm, locals: *locals, pos: *pos, .. Block::default() };
                    b.code.init(code.clone());
                    Cc::new(b)
                }
            }
        ).collect::<Vec<Cc<Block>>>();
        code.blocks.init(blocks_derv);
        code
    }
}
impl Trace for Code {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing Code");
    }
}

// Block
#[derive(Default, Debug)]
pub struct Block {
    pub typ:u8, pub imm:bool, pub locals:usize, pub pos:usize,
    pub code:LateInit<Cc<Code>>,
}
impl Trace for Block {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing Code");
    }
}

// Env (Unboxed)
#[derive(Default,Debug)]
pub struct EnvUnboxed {
    pub parent:Option<Env>,
    pub vars:   Vec<Vh>,
}
impl Trace for EnvUnboxed {
    fn trace(&self, tracer: &mut Tracer) {
        panic!("clearing env");
    }
}
#[derive(Clone,Default,Debug)]
pub struct Env(Cc<Mutex<EnvUnboxed>>);
impl Env {
    pub fn new(env: EnvUnboxed) -> Self {
        Env(Cc::new(Mutex::new(env)))
    }
    pub fn get(&self,id: usize) -> V {
        match self {
            Env(arc) => {
                let guard = arc.lock().unwrap();
                debug!("slot is {:?}",(*guard).vars[id]);
                let vh = &(*guard).vars[id];
                match vh {
                    Vh::V(v) => v.clone(),
                    _ => panic!("can't get unset slot"),
                }
            },
        }
    }
    pub fn set(&self,id: usize,v: V) -> V {
        match self {
            Env(arc) => {
                let mut guard = arc.lock().unwrap();
                debug!("slot is {:?}",(*guard).vars[id]);
                (*guard).vars[id] = Vh::V(v.clone());
                v
            },
        }
    }
}

pub struct BlockInst {
    typ:   u8,
    def:   Cc<Block>,
    parent:Env,
    args:  Vec<Vn>,
}

#[derive(Default,Debug)]
pub struct State {
    pub root: Env,
}
impl State {
    pub fn new(block: &Cc<Block>) -> Self {
        debug!("block {}",block.locals);
        let mut vars: Vec<Vh> = Vec::with_capacity(block.locals);
        vars.resize_with(block.locals, || Vh::None);
        let env = EnvUnboxed {parent: None, vars: vars};
        Self {root: Env::new(env) }
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

// Utility fns
pub fn set(d: bool,is: Vs,vs: Vs) -> V {
    match (is,vs) {
        (Vs::Slot(env,id),Vs::Ref(v)) => { env.set(id,v) },
        _ => panic!("can only set slots"),
    }
}
pub fn new_scalar(n: f64) -> V {
    Cc::new(Vu::Scalar(n))
}
