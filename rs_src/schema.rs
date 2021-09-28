use std::sync::Mutex;
use once_cell::sync::OnceCell;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};
use rustler::{Encoder};
use crate::ebqn::vm;
use std::sync::Arc;

rustler::atoms!{ok}

// Traits
pub trait Calleable {
    fn call(&self,x: Vn,w: Vn) -> Vs;
}

// Value (unboxed)
#[derive(Debug,Clone)]
pub enum Vu {
    Scalar(f64),
    BlockInst(BlockInst),
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
            Vu::BlockInst(b) => panic!("can't encode blockinst to BEAM"),
        }
    }
}
impl Calleable for Cc<Vu> {
    fn call(&self,x: Vn, w: Vn) -> Vs {
        match &**self {
            Vu::BlockInst(b) => {
                assert!(b.typ == 0);
                let slots =
                    match &b.args {
                        None => {
                            vec![Vh::V(self.clone()),none_or_clone(&x),none_or_clone(&w)]
                        },
                        Some(args) => {
                            let mut v: Vec<Vh> = vec![Vh::V(self.clone()),none_or_clone(&x),none_or_clone(&w)];
                            let mut a = args.iter().map(|b| Vh::V(b.as_ref().unwrap().clone())).collect::<Vec<Vh>>();
                            v.append(&mut a);
                            v
                        },
                    };
                let env = Env::new(Some(b.parent.clone()),&b.def,Some(slots));
                let (pos,_locals) =
                    match &b.def.body {
                        Body::Imm(body) => b.def.code.bodies[*body],
                        Body::Defer(mon,dya) => {
                            match (&x,&w) {
                                (None,None) => panic!("no args for calling deferred blockinst"),
                                (Some(_),None) => b.def.code.bodies[mon[0]],
                                (Some(_),Some(_)) => b.def.code.bodies[dya[0]],
                                _ => panic!("bad call arity"),
                            }
                        },
                    };
                vm(&env,&b.def.code,&b.def,pos,Vec::new())
            },
            Vu::Scalar(n) => Vs::Ref(self.clone()),
            _ => panic!("no call fn for type"),
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

#[derive(Debug,Clone)]
pub enum Body {
    Imm(usize),
    Defer(Vec<usize>,Vec<usize>),
}

// Code
#[derive(Default,Debug)]
pub struct Code {
    pub bc:    Vec<usize>,
    pub objs:  Vec<V>,
    pub bodies:Vec<(usize,usize)>,
    pub blocks:LateInit<Vec<Cc<Block>>>,
}
impl Code {
    pub fn new(bc: Vec<usize>,objs: Vec<V>,blocks_raw: Vec<(u8,bool,Body)>,bodies: Vec<(usize,usize)>) -> Cc<Self> {
        let code = Cc::new(Self {bc: bc, objs: objs, bodies: bodies, blocks: LateInit::default(), });
        let blocks_derv = blocks_raw.iter().map(|block|
            match block {
                (typ,imm,body) => {
                        let b = Block { typ: *typ, imm: *imm, body: (*body).clone(), code: LateInit::default(), };
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
#[derive(Debug)]
pub struct Block {
    pub typ:u8, pub imm:bool, pub body: Body,
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
    pub fn new(parent: Option<Env>,block: &Cc<Block>,args: Option<Vec<Vh>>) -> Self {
        let (pos,locals) =
            match &block.body {
                Body::Imm(b) => block.code.bodies[*b],
                Body::Defer(mon,dya) => {
                    let arity =
                        match &args {
                            None => panic!("no fn args supplied for deferred block"),
                            Some(v) => v.len(),
                        };
                    match arity {
                        2 => block.code.bodies[mon[0]],
                        3 => block.code.bodies[dya[0]],
                        n => panic!("invalid args supplied for deferred block {}",n),
                    }
                },
            };
        debug!("initializing block env of size {:?}",locals);
        let vars =
            match args {
                None => {
                    let mut v: Vec<Vh> = Vec::with_capacity(locals);
                    v.resize_with(locals, || Vh::None);
                    v
                },
                Some(mut v) => {
                    v.resize_with(locals, || Vh::None);
                    v
                },
            };
        let env = EnvUnboxed {parent: parent, vars: vars};
        Self(Cc::new(Mutex::new(env)))
    }
    pub fn get(&self,id: usize) -> V {
        match self {
            Env(arc) => {
                let guard = arc.lock().unwrap();
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
                debug!("setting slot id {}",id);
                let mut guard = arc.lock().unwrap();
                (*guard).vars[id] = Vh::V(v.clone());
                v
            },
        }
    }
}

#[derive(Debug,Clone)]
pub struct BlockInst {
    typ:   u8,
    def:   Cc<Block>,
    parent:Env,
    args:  Option<Vec<Vn>>,
}
impl BlockInst {
    pub fn new(env: Env,code: Cc<Code>, typ: u8, block: Cc<Block>, args: Option<Vec<Vn>>) -> Self {
        BlockInst {typ: typ, def: block, parent: env, args: args }
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
    Cc::new(Vu::Scalar(n as f64))
}
pub fn none_or_clone(vn: &Vn) -> Vh {
    match vn {
        None => Vh::None,
        Some(v) => Vh::V(v.clone()),
    }
}
