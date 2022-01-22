use std::ops::Deref;
use std::cell::UnsafeCell;
use std::ptr;
use bacon_rajan_cc::Cc;
use crate::ebqn::vm;
use crate::late_init::LateInit;
//use log::{debug, trace, error, log_enabled, info, Level};
use enum_as_inner::EnumAsInner;
use num_traits::{cast::FromPrimitive};

// Traits
pub trait Calleable {
    fn call(&self,stack:&mut Stack,arity:usize,x: Vn,w: Vn) -> Vs;
}
pub trait Decoder {
    fn to_f64(&self) -> f64;
}
pub trait Stacker {
    fn push2(&mut self,v: Vs) -> &Self;
}

#[derive(Clone)]
pub struct Fn(pub fn(usize,Vn,Vn) -> Vs);
impl PartialEq for Fn {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize
    }
}
#[derive(Clone)]
pub struct R1(pub fn(&mut Stack,usize,Vn,Vn,Vn) -> Vs);
impl PartialEq for R1 {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize
    }
}
#[derive(Clone)]
pub struct R2(pub fn(&mut Stack,usize,Vn,Vn,Vn,Vn) -> Vs);
impl PartialEq for R2 {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize
    }
}

// Value (unboxed)
#[derive(Debug,Clone,PartialEq,EnumAsInner)]
pub enum V {
    Scalar(f64),
    Char(char),
    BlockInst(Cc<BlockInst>,Option<usize>),
    UserMd1(Cc<BlockInst>,Cc<D1>,Option<usize>),
    UserMd2(Cc<BlockInst>,Cc<D2>,Option<usize>),
    Nothing,
    A(Cc<A>),
    Fn(Fn,Option<usize>),                          // X, W
    R1(R1,Option<usize>),                          // F, X, W
    R2(R2,Option<usize>),                          // F, G, X, W
    D1(Cc<D1>,Option<usize>),                      // M, F
    D2(Cc<D2>,Option<usize>),                      // M, F, G
    Tr2(Cc<Tr2>,Option<usize>),
    Tr3(Cc<Tr3>,Option<usize>),
}
impl V {
    pub fn is_fn(&self) -> bool {
        match self {
            V::BlockInst(_b,_prim) => true,
            V::UserMd1(_b,_a,_prim) => true,
            V::UserMd2(_b,_a,_prim) => true,
            V::Tr2(_tr2,_prim) => true,
            V::Tr3(_tr3,_prim) => true,
            _ => false,
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
        match self {
            V::Scalar(n) => *n,
            V::Char(c) => f64::from(u32::from(*c)),
            V::BlockInst(_b,_prim) => panic!("can't decode blockinst to RUST"),
            V::UserMd1(_b,_a,_prim) => panic!("can't encode UserMd1 to BEAM"),
            V::UserMd2(_b,_a,_prim) => panic!("can't encode UserMd2 to BEAM"),
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
    fn call(&self,stack:&mut Stack,arity:usize,x: Vn,w: Vn) -> Vs {
        match self {
            V::UserMd1(b,mods,_prim) => {
                let D1(m,f) = mods.deref();
                let args = vec![Some(self.clone()),x.none_or_clone(),w.none_or_clone(),Some(m.clone()),Some(f.clone())];
                let env = Env::new(Some(b.parent.clone()),&b.def,arity,Some(args));
                let pos = body_pos(b,arity);
                vm(&env,&b.def.code,pos,stack)
            },
            V::UserMd2(b,mods,_prim) => {
                let D2(m,f,g) = mods.deref();
                let args = vec![Some(self.clone()),x.none_or_clone(),w.none_or_clone(),Some(m.clone()),Some(f.clone()),Some(g.clone())]; // cloning args is slow
                let env = Env::new(Some(b.parent.clone()),&b.def,arity,Some(args)); // creating a new env is slow
                let pos = body_pos(b,arity);
                vm(&env,&b.def.code,pos,stack)
            },
            V::BlockInst(b,_prim) => {
                let args = vec![Some(self.clone()),x.none_or_clone(),w.none_or_clone()];
                let env = Env::new(Some(b.parent.clone()),&b.def,arity,Some(args));
                let pos = body_pos(b,arity);
                vm(&env,&b.def.code,pos,stack)
            },
            V::Scalar(n) => Vs::V(V::Scalar(*n)),
            V::Char(c) => Vs::V(V::Char(*c)),
            V::Fn(f,_prim) => f.0(arity,x,w),
            V::R1(_f,_prim) => panic!("can't call r1"),
            V::R2(_f,_prim) => panic!("can't call r2"),
            V::D1(d1,_prim) => {
                let D1(m,f) = d1.deref();
                let r =
                match m {
                    V::R1(r1,_prim) => r1.0(stack,arity,Vn(Some(f)),x,w),
                    _ => panic!("can only call raw1 mods in derv1"),
                };
                r
            },
            V::D2(d2,_prim) => {
                let D2(m,f,g) = d2.deref();
                match m {
                    V::R2(r2,_prim) => r2.0(stack,arity,Vn(Some(f)),Vn(Some(g)),x,w),
                    _ => panic!("can only call raw2 mods in derv2"),
                }
            },
            V::Tr2(tr,_prim) => {
                let Tr2(g,h) = tr.deref();
                let r = h.call(stack,arity,x,w);
                g.call(stack,1,Vn(*Some(&r.as_v()).unwrap()),Vn(None))
            },
            V::Tr3(tr,_prim) => {
                let Tr3(f,g,h) = tr.deref();
                let r =
                    match arity { // TODO this match might not be necessary
                        1 => h.call(stack,arity,Vn(x.0),Vn(None)),
                        2 => h.call(stack,arity,Vn(x.0),Vn(w.0)),
                        _ => panic!("illegal arity"),
                    };
                let l = f.call(stack,arity,Vn(x.0),Vn(w.0));
                g.call(stack,2,Vn(Some(&r.as_v().unwrap())),Vn(Some(&l.as_v().unwrap())))
            },
            V::A(_) => Vs::V(self.clone()),
            V::Nothing => Vs::V(V::Nothing),
        }
    }
}

// Value (Optional)
pub struct Vn<'a>(pub Option<&'a V>);
impl<'a> Vn<'a> {
    fn none_or_clone(&self) -> Vh {
        match self.deref().0 {
            None => Some(V::Nothing),
            Some(v) => Some(v.clone()),
        }

    }
}

// Value (boxed on the stack)
#[derive(Debug,Clone,EnumAsInner)]
pub enum Vs {
    V(V),
    Slot(Env,usize),
    Ar(Ar),
    Nothing
}
impl Vs {
    pub fn get(&self) -> V {
        match self {
            Vs::Slot(env,id) => env.get(*id),
            Vs::Ar(a) => {
                let shape = vec![a.r.len() as usize];
                let ravel = a.r.iter().map(|e| match e { Vr::Slot(env,id) => env.get(*id), }).collect::<Vec<V>>();
                V::A(Cc::new(A::new(ravel,shape)))
            },
            _ => panic!("can only resolve slots or ref arrays"),
        }
    }
    pub fn set(&self,d: bool,vs: Vs) -> V {
        match self {
            Vs::Slot(env,id) => { let v = vs.into_v().unwrap(); env.set(d,*id,&v); v },
            Vs::Ar(a) => {
                let v = vs.into_v().unwrap();
                let arr =
                    match &v {
                        V::A(arr) => arr.clone(),
                        _ => panic!("can only set array of refs if value is an array"),
                    };
                a.r.iter().enumerate().for_each(|(i,e)|
                    match e {
                        Vr::Slot(env,id) => {
                            env.set(d,*id,&arr.r[i]);
                        },
                    }
                );
                v
            },
            _ => panic!("can only set slots"),
        }
    }
}
impl Default for Vs {
    fn default() -> Self {
        Vs::Nothing
    }
}

// Value (boxed on the heap)
pub type Vh = Option<V>;

// value reference
#[derive(Debug,Clone)]
pub enum Vr {
    Slot(Env,usize),
}

// Stack
pub struct Stack {
    pub s: Vec<Vs>,
    pub fp: usize,
}
impl Stack {
    pub fn new() -> Self {
        Self { s: Vec::with_capacity(128), fp: 0 }
    }
}
impl Stacker for Vec<Vs> {
    fn push2(&mut self,v: Vs) -> &Self {
        unsafe {
            let l = self.len();
            let end = self.as_mut_ptr().add(l);
            ptr::write(end,v);
            self.set_len(l+1);
            self
        }
    }
}

// Body
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
        let blocks_derv = blocks_raw.into_iter().map(|block|
            match block {
                (typ,imm,body) => {
                        let b = Block { typ: typ, imm: imm, body: body, code: LateInit::default(), };
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
    pub vars:   UnsafeCell<Vec<Vh>>,
}

#[derive(Clone,Debug)]
pub struct Env(Cc<EnvUnboxed>);
impl Env {
    pub fn new(parent: Option<Env>,block: &Cc<Block>,arity: usize,args: Option<Vec<Vh>>) -> Self {
        // index into bodies with unsafe code.
        // we are assuming the compiler has produced correct body indexing
        // environment creation is in the outskirts of the hot-path.
        // as such, this might be changed back to using safe code depending
        // on how headers are implemented
        let (_pos,locals) =
            match &block.body {
                Body::Imm(b) => unsafe {*block.code.bodies.get_unchecked(*b) },
                Body::Defer(mon,dya) => {
                    match arity {
                        1 => unsafe { *block.code.bodies.get_unchecked(mon[0]) },
                        2 => unsafe { *block.code.bodies.get_unchecked(dya[0]) },
                        n => panic!("invalid arity for deferred block {}",n),
                    }
                },
            };
        let vars =
            match args {
                None => {
                    let mut v: Vec<Vh> = Vec::with_capacity(locals);
                    v.resize_with(locals, || None);
                    v
                },
                Some(mut v) => {
                    v.resize_with(locals, || None);
                    v
                },
            };
        let env = EnvUnboxed {parent: parent, vars: UnsafeCell::new(vars) };
        Self(Cc::new(env))
    }
    // get, set, and get_drop use unsafe code
    // we are assuming that the compiler is correctly indexing locals
    // we are using unsafe because interacting with the heap is in the hot-path of the vm
    pub fn get(&self,id: usize) -> V {
        match self {
            Env(e) => {
                match unsafe { &(*e.vars.get()).get_unchecked(id) } {
                    Some(v) => v.clone(),
                    None => panic!("heap slot is undefined"),
                }
            },
        }
    }
    pub fn set(&self,d: bool,id: usize,v: &V) {
        match self {
            Env(e) => {
                assert_eq!(d,unsafe { &(*e.vars.get()).get_unchecked(id) }.is_none());
                unsafe { *(*e.vars.get()).get_unchecked_mut(id) = Some(v.clone())};
            },
        }
    }
    pub fn get_drop(&self,id: usize) -> V {
        match self {
            Env(e) => {
                let r =
                    match unsafe { &(*e.vars.get()).get_unchecked(id) } {
                        Some(v) => v.clone(),
                        None => panic!("heap slot is undefined"),
                    };
                unsafe { *(*e.vars.get()).get_unchecked_mut(id) = None };
                r
            },
        }
    }
    pub fn ge(&self,mut i: usize) -> &Env {
        let mut cur = self;
        loop {
            match i {
                0 => break cur,
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
    pub fn call_md1(&self,stack:&mut Stack,arity:usize,args: D1) -> Vs {
        match self.def.imm {
            false => {
                let r = Vs::V(V::UserMd1(Cc::new(BlockInst::new(self.parent.clone(),self.def.clone())),Cc::new(args),None));
                r
            },
            true => {
                let pos = match self.def.body {
                   Body::Imm(b) => {
                        let (p,_l) = self.def.code.bodies[b];
                        p
                    }
                    _ => panic!("body immediacy doesnt match block definition"),
                };
                let D1(m,f) = args;
                let env = Env::new(Some(self.parent.clone()),&self.def,arity,Some(vec![Some(m.clone()),Some(f.clone())]));
                vm(&env,&self.def.code,pos,stack)
            },
        }
    }
    pub fn call_md2(&self,stack:&mut Stack,arity:usize,args: D2) -> Vs {
        match self.def.imm {
            false => {
                let r = Vs::V(V::UserMd2(Cc::new(BlockInst::new(self.parent.clone(),self.def.clone())),Cc::new(args),None));
                r
            },
            true => {
                let pos = match self.def.body {
                   Body::Imm(b) => {
                        let (p,_l) = self.def.code.bodies[b];
                        p
                    }
                    _ => panic!("body immediacy doesnt match block definition"),
                };
                let D2(m,f,g) = args;
                let env = Env::new(Some(self.parent.clone()),&self.def,arity,Some(vec![Some(m.clone()),Some(f.clone()),Some(g.clone())]));
                vm(&env,&self.def.code,pos,stack)
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
        Self(g.into_v().unwrap(),h.into_v().unwrap())
    }
}
#[derive(Debug,Clone,PartialEq)]
pub struct Tr3(pub V,pub V,pub V);
impl Tr3 {
    pub fn new(f: Vs,g: Vs,h: Vs) -> Self {
        Self(f.into_v().unwrap(),g.into_v().unwrap(),h.into_v().unwrap())
    }
}

pub struct Runtime(pub A);
pub struct Compiler(pub Cc<BlockInst>);
pub struct Prog(pub Cc<Code>);

// Utility fns
pub fn new_scalar<T: Decoder>(n: T) -> V {
    V::Scalar(n.to_f64())
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
