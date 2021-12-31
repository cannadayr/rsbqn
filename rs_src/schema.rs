use std::ops::Deref;
use std::sync::Mutex;
use cc_mt::Cc;
use rustler::{Encoder};
use crate::ebqn::vm;
use crate::late_init::LateInit;
//use log::{debug, trace, error, log_enabled, info, Level};
use enum_as_inner::EnumAsInner;

rustler::atoms!{ok}

// Traits
pub trait Calleable {
    fn call(&self,arity: usize,x: Vn,w: Vn) -> Vs;
}
pub trait Decoder {
    fn to_f64(&self) -> f64;
}

// Value (unboxed)
#[derive(Debug,Clone,PartialEq,EnumAsInner)]
pub enum V {
    Scalar(f64),
    Char(char),
    BlockInst(Cc<BlockInst>,Option<usize>),
    DervBlockInst(Cc<BlockInst>,Vec<Vn>,Option<usize>),
    Nothing,
    A(Cc<A>),
    Fn(fn(usize,Vn,Vn) -> Vs,Option<usize>),       // X, W
    R1(fn(usize,Vn,Vn,Vn) -> Vs,Option<usize>),    // F, X, W
    R2(fn(usize,Vn,Vn,Vn,Vn) -> Vs,Option<usize>), // F, G, X, W
    D1(Cc<D1>,Option<usize>),                      // M, F
    D2(Cc<D2>,Option<usize>),                      // M, F, G
    Tr2(Cc<Tr2>,Option<usize>),
    Tr3(Cc<Tr3>,Option<usize>),
}
impl V {
    pub fn to_array(&self) -> &A {
        match self {
            V::A(a) => a,
            _ => panic!("can't convert to array"),
        }
    }
    pub fn is_fn(&self) -> bool {
        match self {
            V::BlockInst(_b,_prim) => true,
            V::DervBlockInst(_b,_a,_prim) => true,
            V::Tr2(_tr2,_prim) => true,
            V::Tr3(_tr3,_prim) => true,
            _ => false,
        }
    }
}
impl Encoder for V {
    fn encode<'a>(&self, env: rustler::Env<'a>) -> rustler::Term<'a> {
        match self {
            V::Scalar(n) => n.encode(env),
            V::Char(c) => panic!("can't encode char to BEAM"),
            V::BlockInst(_b,_prim) => panic!("can't encode blockinst to BEAM"),
            V::DervBlockInst(_b,_a,_prim) => panic!("can't encode dervblockinst to BEAM"),
            V::Nothing => panic!("can't encode nothing to BEAM"),
            V::A(_a) => panic!("can't encode array to BEAM"),
            V::Fn(_a,_prim) => panic!("can't encode fn to BEAM"),
            V::R1(_f,_prim) => panic!("can't encode r1 to BEAM"),
            V::R2(_f,_prim) => panic!("can't encode r2 to BEAM"),
            V::D1(_d1,_prim) => panic!("can't encode d1 to BEAM"),
            V::D2(_d2,_prim) => panic!("can't encode d2 to BEAM"),
            V::Tr2(_tr2,_prim) => panic!("can't encode train2 to BEAM"),
            V::Tr3(_tr3,_prim) => panic!("can't encode train3 to BEAM"),
        }
    }
}
impl Decoder for i64 {
    fn to_f64(&self) -> f64 {
        *self as f64
    }
}
impl Decoder for f64 {
    fn to_f64(&self) -> f64 {
        *self
    }
}
impl Decoder for V {
    fn to_f64(&self) -> f64 {
        match self.deref() {
            V::Scalar(n) => *n,
            // a proper u32 to char conversions requires the unstable feature 'assoc_char_funcs'
            // https://github.com/rust-lang/rust/issues/71763
            // use u8's for now
            V::Char(c) => *c as u8 as f64,
            V::BlockInst(_b,_prim) => panic!("can't decode blockinst to RUST"),
            V::DervBlockInst(_b,_a,_prim) => panic!("can't encode dervblockinst to BEAM"),
            V::Nothing => panic!("can't decode nothing to BEAM"),
            V::A(_a) => panic!("can't decode array to RUST"),
            V::Fn(_a,_prim) => panic!("can't decode fn to RUST"),
            V::R1(_f,_prim) => panic!("can't decode r1 to RUST"),
            V::R2(_f,_prim) => panic!("can't decode r2 to RUST"),
            V::D1(_d1,_prim) => panic!("can't decode d1 to BEAM"),
            V::D2(_d2,_prim) => panic!("can't decode d2 to BEAM"),
            V::Tr2(_tr2,_prim) => panic!("can't decode train2 to RUST"),
            V::Tr3(_tr3,_prim) => panic!("can't decode train3 to RUST"),
        }
    }
}
impl Calleable for V {
    fn call(&self,arity:usize,x: Vn,w: Vn) -> Vs {
        match self.deref() {
            V::DervBlockInst(b,mods,_prim) => {
                let mut args = vec![Vh::V(self.clone()),none_or_clone(&x),none_or_clone(&w)];
                let mut m = mods.iter().map(|e| Vh::V(e.as_ref().unwrap().clone())).collect::<Vec<Vh>>();
                args.append(&mut m);
                let env = Env::new(Some(b.parent.clone()),&b.def,arity,Some(args));
                let pos = body_pos(b,arity);
                vm(&env,&b.def.code,pos,Vec::new())
            },
            V::BlockInst(b,_prim) => {
                let mut args = vec![Vh::V(self.clone()),none_or_clone(&x),none_or_clone(&w)];
                let env = Env::new(Some(b.parent.clone()),&b.def,arity,Some(args));
                let pos = body_pos(b,arity);
                vm(&env,&b.def.code,pos,Vec::new())
            },
            V::Scalar(_n) => Vs::V(self.clone()),
            V::Char(_c) => Vs::V(self.clone()),
            V::Fn(f,_prim) => f(arity,x,w),
            V::R1(_f,_prim) => panic!("can't call r1"),
            V::R2(_f,_prim) => panic!("can't call r2"),
            V::D1(d1,_prim) => {
                let D1(m,f) = d1.deref();
                match m {
                    V::R1(r1,_prim) => r1(arity,Some(f.clone()),x,w),
                    _ => panic!("can only call raw1 mods in derv1"),
                }
            },
            V::D2(d2,_prim) => {
                let D2(m,f,g) = d2.deref();
                match m {
                    V::R2(r2,_prim) => r2(arity,Some(f.clone()),Some(g.clone()),x,w),
                    _ => panic!("can only call raw2 mods in derv2"),
                }
            },
            V::Tr2(tr,_prim) => {
                let Tr2(g,h) = tr.deref();
                let r = h.call(arity,x,w);
                g.call(1,Some(r.to_ref().clone()),None)
            },
            V::Tr3(tr,_prim) => {
                let Tr3(f,g,h) = tr.deref();
                let r =
                    match arity {
                        1 => h.call(arity,Some((*x.as_ref().unwrap()).clone()),None),
                        2 => h.call(arity,Some((*x.as_ref().unwrap()).clone()),Some((*w.as_ref().unwrap()).clone())),
                        _ => panic!("illegal arity"),
                    };
                let l = f.call(arity,x,w);
                g.call(2,Some(r.to_ref().clone()),Some(l.to_ref().clone()))
            },
            V::A(_) => Vs::V(self.clone()),
            V::Nothing => Vs::V(V::Nothing),
        }
    }
}

// Value (Optional)
pub type Vn = Option<V>;

// Value (boxed on the stack)
#[derive(Debug,Clone,EnumAsInner)]
pub enum Vs {
    V(V),
    Slot(Env,usize),
    Ar(Ar)
}
impl Vs {
    pub fn to_ref(&self) -> &V {
        match self {
            Vs::V(v) => v,
            _ => panic!("can't convert to ref"),
        }
    }
    pub fn get(&self) -> V {
        match self {
            Vs::Slot(env,id) => env.get(*id),
            Vs::Ar(a) => {
                let shape = vec![a.r.len() as usize];
                let ravel =
                    a.r.iter().map(|e| match e { Vr::Slot(env,id) => env.get(*id), }).collect::<Vec<V>>();
                V::A(Cc::new(A::new(ravel,shape)))
            },
            _ => panic!("can only resolve slots or ref arrays"),
        }
    }
}

// Value (boxed on the heap)
#[derive(Debug)]
pub enum Vh {
    Undefined,
    V(V),
}

// value reference
#[derive(Debug,Clone)]
pub enum Vr {
    Slot(Env,usize),
}

#[derive(Debug,Clone,PartialEq)]
pub enum Body {
    Imm(usize),
    Defer(Vec<usize>,Vec<usize>),
}

// Code
#[derive(Default,Debug,PartialEq)]
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

// Block
#[derive(Debug,PartialEq)]
pub struct Block {
    pub typ:u8, pub imm:bool, pub body: Body,
    pub code:LateInit<Cc<Code>>,
}

// Env (Unboxed)
#[derive(Default,Debug)]
pub struct EnvUnboxed {
    pub parent:Option<Env>,
    pub vars:   Mutex<Vec<Vh>>,
}

#[derive(Clone,Default,Debug)]
pub struct Env(Cc<EnvUnboxed>);
impl Env {
    pub fn new(parent: Option<Env>,block: &Cc<Block>,arity: usize,args: Option<Vec<Vh>>) -> Self {
        let (_pos,locals) =
            match &block.body {
                Body::Imm(b) => block.code.bodies[*b],
                Body::Defer(mon,dya) => {
                    match arity {
                        1 => block.code.bodies[mon[0]],
                        2 => block.code.bodies[dya[0]],
                        n => panic!("invalid arity for deferred block {}",n),
                    }
                },
            };
        let vars =
            match args {
                None => {
                    let mut v: Vec<Vh> = Vec::with_capacity(locals);
                    v.resize_with(locals, || Vh::Undefined);
                    v
                },
                Some(mut v) => {
                    v.resize_with(locals, || Vh::Undefined);
                    v
                },
            };
        let env = EnvUnboxed {parent: parent, vars: Mutex::new(vars) };
        Self(Cc::new(env))
    }
    pub fn get(&self,id: usize) -> V {
        match self {
            Env(e) => {
                let guard = e.vars.lock().unwrap();
                let vh = &(*guard)[id];
                match vh {
                    Vh::V(v) => v.clone(),
                    Vh::Undefined => panic!("heap slot is undefined"),
                    _ => panic!("can't get unset slot"),
                }
            },
        }
    }
    pub fn set(&self,d: bool,id: usize,v: &V) {
        match self {
            Env(e) => {
                let mut guard = e.vars.lock().unwrap();
                let vh = &(*guard)[id];
                assert_eq!(d,match vh {
                    Vh::Undefined => true,
                    _ => false,
                });
                (*guard)[id] = Vh::V((*v).clone());
            },
        }
    }
    pub fn ge(&self,mut i: usize) -> Env {
        let mut cur = self;
        loop {
            match i {
                0 => break (*cur).clone(),
                _ => {
                    i-=1;
                    cur = cur.0.parent.as_ref().unwrap();
                },
            }
        }
    }
}

#[derive(Debug,Clone)]
pub struct BlockInst {
    pub def:   Cc<Block>,
    parent:Env,
}
impl BlockInst {
    pub fn new(env: Env,block: Cc<Block>) -> Self {
        Self {def: block, parent: env }
    }
    pub fn call_block(&self,arity:usize,args: Vec<Vn>) -> Vs {
        match self.def.imm {
            false => {
                Vs::V(V::DervBlockInst(Cc::new(BlockInst::new(self.parent.clone(),self.def.clone())),args,None))
            },
            true => {
                let pos = match self.def.body {
                   Body::Imm(b) => {
                        let (p,_l) = self.def.code.bodies[b];
                        p
                    }
                    _ => panic!("body immediacy doesnt match block definition"),
                };
                let a = args.iter().map(|v| Vh::V(v.as_ref().unwrap().clone())).collect::<Vec<Vh>>();
                let env = Env::new(Some(self.parent.clone()),&self.def,arity,Some(a));
                vm(&env,&self.def.code,pos,Vec::new())
            },
        }
    }
}
impl PartialEq for BlockInst {
    fn eq(&self, other: &Self) -> bool {
        Cc::ptr_eq(&self.def,&other.def) && Cc::ptr_eq(&self.parent.0,&other.parent.0)
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct A {
    pub r: Vec<V>,
    pub sh: Vec<usize>,
}
impl A {
    pub fn new(r: Vec<V>,sh: Vec<usize>) -> Self {
        Self { r: r, sh: sh }
    }
}

// array of references. rank-1 for now.
#[derive(Debug,Clone)]
pub struct Ar {
    r: Vec<Vr>,
}
impl Ar {
    pub fn new(r: Vec<Vr>) -> Self {
        Self { r: r }
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct D1(pub V,pub V);
impl D1 {
    pub fn new(m: V, f: V) -> Self {
        Self(m,f)
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct D2(pub V,pub V,pub V);
impl D2 {
    pub fn new(m: V, f: V,g: V) -> Self {
        Self(m,f,g)
    }
}
#[derive(Debug,Clone,PartialEq)]
pub struct Tr2(pub V,pub V);
impl Tr2 {
    pub fn new(g: Vs,h: Vs) -> Self {
        Self(g.to_ref().clone(),h.to_ref().clone())
    }
}
#[derive(Debug,Clone,PartialEq)]
pub struct Tr3(pub V,pub V,pub V);
impl Tr3 {
    pub fn new(f: Vs,g: Vs,h: Vs) -> Self {
        Self((*f.to_ref()).clone(),(*g.to_ref()).clone(),(*h.to_ref()).clone())
    }
}

// Utility fns
pub fn set(d: bool,is: Vs,vs: Vs) -> V {
    match (is,vs) {
        (Vs::Slot(env,id),Vs::V(v)) => { env.set(d,id,&v); v },
        (Vs::Ar(a),Vs::V(v)) => {
            let arr =
                match &v {
                    V::A(arr) => arr.clone(),
                    _ => panic!("can only set array of refs if value is an array"),
                };
            a.r.into_iter().enumerate().for_each(|(i,e)|
                match e {
                    Vr::Slot(env,id) => {
                        env.set(d,id,&arr.r[i]);
                    },
                }
            );
            v
        },
        _ => panic!("can only set slots"),
    }
}
pub fn new_scalar<T: Decoder>(n: T) -> V {
    V::Scalar(n.to_f64())
}
pub fn none_or_clone(vn: &Vn) -> Vh {
    match vn {
        None => Vh::V(V::Nothing),
        Some(v) => Vh::V(v.clone()),
    }
}
pub fn body_pos(b: &Cc<BlockInst>,arity: usize) -> usize {
    let (pos,_locals) =
        match &b.def.body {
            Body::Imm(body) => b.def.code.bodies[*body],
            Body::Defer(mon,dya) => {
                match arity {
                    1 => b.def.code.bodies[mon[0]],
                    2 => b.def.code.bodies[dya[0]],
                    _ => panic!("bad call arity"),
                }
            },
        };
    pos
}
pub fn new_char(n: char) -> V {
    V::Char(n)
}
pub fn new_string(n: &str) -> V {
    let ravel = n.to_string().chars().map(|c| V::Char(c)).collect::<Vec<V>>();
    let shape = vec![ravel.len() as usize];
    V::A(Cc::new(A::new(ravel,shape)))
}
