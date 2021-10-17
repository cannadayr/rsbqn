use std::sync::Mutex;
use cc_mt::Cc;
use rustler::{Encoder};
use crate::ebqn::vm;
use crate::late_init::LateInit;
//use log::{debug, trace, error, log_enabled, info, Level};

rustler::atoms!{ok}

// Traits
pub trait Calleable {
    fn call(&self,arity: usize,x: Vn,w: Vn) -> Vs;
}

// Value (unboxed)
#[derive(Debug,Clone)]
pub enum Vu {
    Scalar(f64),
    BlockInst(BlockInst),
    A(A),
    Tr2(Tr2),
    Tr3(Tr3),
}
impl Encoder for Vu {
    fn encode<'a>(&self, env: rustler::Env<'a>) -> rustler::Term<'a> {
        match self {
            Vu::Scalar(n) => n.encode(env),
            Vu::BlockInst(_b) => panic!("can't encode blockinst to BEAM"),
            Vu::A(_a) => panic!("can't encode array to BEAM"),
            Vu::Tr2(_tr2) => panic!("can't encode train2 to BEAM"),
            Vu::Tr3(_tr3) => panic!("can't encode train3 to BEAM"),
        }
    }
}
impl Calleable for Cc<Vu> {
    fn call(&self,arity: usize,x: Vn,w: Vn) -> Vs {
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
                let env = Env::new(Some(b.parent.clone()),&b.def,arity,Some(slots));
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
                vm(&env,&b.def.code,pos,Vec::new())
            },
            Vu::Scalar(_n) => Vs::V(self.clone()),
            Vu::Tr2(Tr2(g,h)) => {
                let r = h.call(arity,x,w);
                g.call(1,Some(r.to_ref().clone()),None)
            },
            Vu::Tr3(Tr3(f,g,h)) => {
                let r =
                    match (&x,&w) {
                        (_,None) => h.call(arity,Some((*x.as_ref().unwrap()).clone()),None),
                        (_,_) => h.call(arity,Some((*x.as_ref().unwrap()).clone()),Some((*w.as_ref().unwrap()).clone())),
                    };
                let l = f.call(arity,x,w);
                g.call(2,Some(r.to_ref().clone()),Some(l.to_ref().clone()))
            },
            Vu::A(_) => Vs::V(self.clone()),
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
                let shape = vec![Cc::new(Vu::Scalar(a.r.len() as f64))];
                let ravel =
                    a.r.iter().map(|e| match e { Vr::Slot(env,id) => env.get(*id), }).collect::<Vec<V>>();
                Cc::new(Vu::A(A::new(ravel,shape)))
            },
            _ => panic!("can only resolve slots or ref arrays"),
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

// value reference
#[derive(Debug,Clone)]
pub enum Vr {
    Slot(Env,usize),
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

// Block
#[derive(Debug)]
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
                    v.resize_with(locals, || Vh::None);
                    v
                },
                Some(mut v) => {
                    v.resize_with(locals, || Vh::None);
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
                    Vh::None|Vh::Undefined => true,
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
    pub typ:   u8,
    def:   Cc<Block>,
    parent:Env,
    args:  Option<Vec<Vn>>,
}
impl BlockInst {
    pub fn new(env: Env,typ: u8, block: Cc<Block>, args: Option<Vec<Vn>>) -> Self {
        Self {typ: typ, def: block, parent: env, args: args }
    }
    pub fn call_block(&self,arity:usize,args: Vec<Vn>) -> Vs {
        match self.def.imm {
            false => {
                Vs::V(Cc::new(Vu::BlockInst(BlockInst::new(self.parent.clone(),0,self.def.clone(),Some(args)))))
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

#[derive(Debug,Clone)]
pub struct A {
    pub r: Vec<V>,
    pub sh: Vec<V>,
}
impl A {
    pub fn new(r: Vec<V>,sh: Vec<V>) -> Self {
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

#[derive(Debug,Clone)]
pub struct Tr2(V,V);
impl Tr2 {
    pub fn new(g: Vs,h: Vs) -> Self {
        Self((*g.to_ref()).clone(),(*h.to_ref()).clone())
    }
}
#[derive(Debug,Clone)]
pub struct Tr3(V,V,V);
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
                match &*v {
                    Vu::A(arr) => arr,
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
pub fn new_scalar(n: f64) -> V {
    Cc::new(Vu::Scalar(n as f64))
}
pub fn none_or_clone(vn: &Vn) -> Vh {
    match vn {
        None => Vh::None,
        Some(v) => Vh::V(v.clone()),
    }
}
