use crate::schema::{Env,V,Vu,Vs,Vr,Vn,Vh,Block,BlockInst,Code,Calleable,Body,A,Ar,Tr2,Tr3,set,new_scalar,none_or_clone,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};
use std::panic;
use crate::test::{bytecode};

fn ge(env: Env,i: usize) -> Env {
    match i {
        0 => env,
        _ => panic!("ge not implemented for i > 0")
    }
}

fn call(arity: usize,a: Vn,x: Vn, w: Vn) -> Vs {
    match a {
        Some(v) => v.call(arity,x,w),
        _ => panic!("unimplemented call"),
    }
}
fn call1(m: V,f: V) -> Vs {
    match &*m {
        Vu::BlockInst(bl) => {
            assert_eq!(1,bl.typ);
            bl.call_block(1,vec![Some(m.clone()),Some(f)])
        },
        _ => panic!("call1 with invalid type"),
    }
}
fn call2(m: V,f: V,g: V) -> Vs {
    match &*m {
        Vu::BlockInst(bl) => {
            assert_eq!(2,bl.typ);
            bl.call_block(2,vec![Some(m.clone()),Some(f),Some(g)])
        },
        _ => panic!("call2 with invalid type"),
    }
}

fn derv(env: Env,code: &Cc<Code>,block: &Cc<Block>) -> Vs {
    debug!("deriving block from body {:?}",block.body);
    match (block.typ,block.imm) {
        (0,true) => {
            debug!("deriving immediate block");
            let child = Env::new(Some(env.clone()),block,0,None);
            let pos = match block.body {
                Body::Imm(b) => {
                    debug!("immediate block body {}",b);
                    let (p,_l) = code.bodies[b];
                    p
                },
                _ => panic!("body immediacy derivation doesnt match block definition"),
            };
            debug!("new child vm with body {:?} and pos {}",block.body,pos);
            vm(&child,code,block,pos,Vec::new())
        },
        (typ,_) => {
            debug!("deriving block instance from body {:?}",block.body);
            let block_inst = BlockInst::new(env.clone(),code.clone(),typ,(*block).clone(),None);
            let r = Vs::V(Cc::new(Vu::BlockInst(block_inst)));
            r
        },
    }
}

fn list(l: Vec<Vs>) -> Vs {
    let shape = vec![Cc::new(Vu::Scalar(l.len() as f64))];
    let ravel = l.into_iter().map(|e|
        match e {
            Vs::V(v) => v,
            _ => panic!("illegal slot passed to list"),
        }
    ).collect::<Vec<V>>();
    Vs::V(Cc::new(Vu::A(A::new(ravel,shape))))
}
fn listr(l: Vec<Vs>) -> Vs {
    let ravel = l.into_iter().map(|e|
        match e {
            Vs::Slot(env,slot) => Vr::Slot(env,slot),
            _ => panic!("illegal non-slot passed to list"),
        }
    ).collect::<Vec<Vr>>();
    Vs::Ar(Ar::new(ravel))
}
pub fn vm(env: &Env,code: &Cc<Code>,block: &Cc<Block>,mut pos: usize,mut stack: Vec<Vs>) -> Vs {
    debug!("new vm with body {:?} and pos {}",block.body,pos);
    loop {
        let op = code.bc[pos];pos+=1;
        debug!("opcode = {}; pos = {}",op,pos);
        match op {
            0 => {
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                stack.push(Vs::V(r))
            },
            1 => {
                let x = code.bc[pos];pos+=1;
                let r = derv(env.clone(),&code,&code.blocks[x]);
                stack.push(r);
            },
            6 => {
                let _ = stack.pop();
            },
            7 => {
                break match stack.len() {
                    1 => {
                        stack.pop().unwrap()
                    },
                    _ => {
                        panic!("stack overflow")
                    }
                };
            },
            11 => {
                let x = code.bc[pos];pos+=1;
                let hd = stack.len() - x;
                let tl = stack.split_off(hd);
                stack.push(list(tl));
            },
            12 => {
                let x = code.bc[pos];pos+=1;
                let hd = stack.len() - x;
                let tl = stack.split_off(hd);
                stack.push(listr(tl));
            },
            16 => {
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(1,Some(f.to_ref().clone()),Some(x.to_ref().clone()),None);
                stack.push(r);
            },
            17 => {
                let w = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(2,Some(f.to_ref().clone()),Some(x.to_ref().clone()),Some(w.to_ref().clone()));
                stack.push(r);
            },
            20 => {
                let g = stack.pop().unwrap();
                let h = stack.pop().unwrap();
                let t = Vs::V(Cc::new(Vu::Tr2(Tr2::new(g,h))));
                stack.push(t);
            },
            21 => {
                let f = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                let h = stack.pop().unwrap();
                let t = Vs::V(Cc::new(Vu::Tr3(Tr3::new(f,g,h))));
                stack.push(t);
            },
            26 => {
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                let r = call1(m.to_ref().clone(),f.to_ref().clone());
                stack.push(r);
            },
            27 => {
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                let r = call2(m.to_ref().clone(),f.to_ref().clone(),g.to_ref().clone());
                stack.push(r);
            },
            33 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                let t = ge(env.clone(),x);
                stack.push(Vs::Slot(t,w))
            },
            34 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                let t = ge(env.clone(),x);
                stack.push(Vs::V(t.get(w)))
            },
            48 => {
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(true,i,v); // rtns a reference to v
                stack.push(Vs::V(r));
            },
            49 => {
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(false,i,v); // rtns a reference to v
                stack.push(Vs::V(r));
            },
            _ => {
                panic!("unreachable op: {}",op);
            }
        }
    }
}

#[rustler::nif]
fn tests() -> NifResult<Atom> {
    bytecode();
    Ok(ok())
}

pub fn run(code: Cc<Code>) -> f64 {
    let root = Env::new(None,&code.blocks[0],0,None);
    let (pos,_locals) =
        match code.blocks[0].body {
            Body::Imm(b) => code.bodies[b],
            Body::Defer(_,_) => panic!("cant run deferred block"),
        };
    debug!("new root with body {:?} and pos {}",code.blocks[0].body,pos);
    let rtn = vm(&root,&code,&code.blocks[0],pos,Vec::new());
    match &**rtn.to_ref() {
        Vu::Scalar(n) => *n,
        Vu::A(a) => panic!("got array w/ shape {:?}",a.sh),
        _ => panic!("run failed"),
    }
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Env>,Vs)> {
    //let code = Code::new(vec![0,0,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,0)]);
    //let root = Env::new(None,&code.blocks[0],None);
    panic!("cant init anything");
    //let rtn = vm(&root,&code,&code.blocks[0],code.blocks[0].pos,Vec::new());
    //Ok((ok(),ResourceArc::new(root),rtn))
}
