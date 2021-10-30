use crate::schema::{Env,V,Vs,Vr,Vn,Block,BlockInst,Code,Calleable,Body,A,Ar,Tr2,Tr3,set,ok,D2};
use crate::prim::{provide};
use crate::code::{r0};
use crate::fmt::{fmt_stack};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use cc_mt::Cc;
use crate::test::{bytecode,prim};
use std::ops::Deref;
//use std::panic;
use log::{debug, trace, error, log_enabled, info, Level};

pub fn call(arity: usize,a: Vn,x: Vn, w: Vn) -> Vs {
    match a {
        Some(v) => v.call(arity,x,w),
        _ => panic!("unimplemented call"),
    }
}
fn call1(m: V,f: V) -> Vs {
    match m {
        V::BlockInst(ref bl) => {
            assert_eq!(1,bl.def.typ);
            bl.call_block(1,vec![Some(m.clone()),Some(f)])
        },
        _ => panic!("call1 with invalid type"),
    }
}
fn call2(m: V,f: V,g: V) -> Vs {
    match m {
        V::BlockInst(ref bl) => {
            assert_eq!(2,bl.def.typ);
            bl.call_block(2,vec![Some(m.clone()),Some(f),Some(g)])
        },
        V::R2(_) => Vs::V(V::D2(Cc::new(D2::new(m,f,g)))),
        _ => panic!("call2 with invalid type"),
    }
}

fn derv(env: Env,code: &Cc<Code>,block: &Cc<Block>) -> Vs {
    match (block.typ,block.imm) {
        (0,true) => {
            let child = Env::new(Some(env.clone()),block,0,None);
            let pos = match block.body {
                Body::Imm(b) => {
                    let (p,_l) = code.bodies[b];
                    p
                },
                _ => panic!("body immediacy derivation doesnt match block definition"),
            };
            vm(&child,code,pos,Vec::new())
        },
        (_typ,_imm) => {
            let block_inst = BlockInst::new(env.clone(),(*block).clone());
            let r = Vs::V(V::BlockInst(Cc::new(block_inst)));
            r
        },
    }
}

fn list(l: Vec<Vs>) -> Vs {
    let shape = vec![l.len() as usize];
    let ravel = l.into_iter().map(|e|
        match e {
            Vs::V(v) => v,
            _ => panic!("illegal slot passed to list"),
        }
    ).collect::<Vec<V>>();
    Vs::V(V::A(Cc::new(A::new(ravel,shape))))
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
pub fn vm(env: &Env,code: &Cc<Code>,mut pos: usize,mut stack: Vec<Vs>) -> Vs {
    loop {
        let op = code.bc[pos];pos+=1;
        match op {
            0 => { // PUSH
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","PUSH",x,r),pos-2),fmt_stack(&stack)); // 2 args
                stack.push(Vs::V(r));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            1 => { // DFND
                let x = code.bc[pos];pos+=1;
                let r = derv(env.clone(),&code,&code.blocks[x]);
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","DFND",x,r),pos-2),fmt_stack(&stack)); // 2 args
                stack.push(r);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            6 => { // POPS
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{}","POPS"),pos-1),fmt_stack(&stack)); // 0 args
                let _ = stack.pop();
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            7 => { // RETN
                break match stack.len() {
                    1 => {
                        debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{}","RETN"),pos-1),fmt_stack(&stack)); // 0 args
                        let rtn = stack.pop().unwrap();
                        debug!("{:<22} :  {}","",fmt_stack(&stack));
                        rtn
                    },
                    _ => {
                        panic!("stack overflow")
                    }
                };
            },
            11 => { // ARRO
                let x = code.bc[pos];pos+=1;
                let hd = stack.len() - x;
                let tl = stack.split_off(hd);
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {}","ARRO",x),pos-2),fmt_stack(&stack)); // 1 args
                stack.push(list(tl));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            12 => { // ARRM
                let x = code.bc[pos];pos+=1;
                let hd = stack.len() - x;
                let tl = stack.split_off(hd);
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {}","ARRM",x),pos-2),fmt_stack(&stack)); // 1 args
                stack.push(listr(tl));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            16|18 => { // FN1C|FN1O
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(1,Some(f.to_ref().clone()),Some(x.to_ref().clone()),None);
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","FN1C",f,x),pos-1),fmt_stack(&stack)); // 2 args
                stack.push(r);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            17|19 => { // FN2C|FN2O
                let w = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(2,Some(f.to_ref().clone()),Some(x.to_ref().clone()),Some(w.to_ref().clone()));
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {} {}","FN2C",w,f,x),pos-1),fmt_stack(&stack)); // 3 args
                stack.push(r);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            20 => { // TR2D
                let g = stack.pop().unwrap();
                let h = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","TR2D",g,h),pos-1),fmt_stack(&stack)); // 2 args
                let t = Vs::V(V::Tr2(Cc::new(Tr2::new(g,h))));
                stack.push(t);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            21 => { // TR3D
                let f = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                let h = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {} {}","TR3D",f,g,h),pos-1),fmt_stack(&stack)); // 3 args
                let t = Vs::V(V::Tr3(Cc::new(Tr3::new(f,g,h))));
                stack.push(t);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            26 => { // MD1C
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","MD1C",f,m),pos-1),fmt_stack(&stack)); // 2 args
                let r = call1(m.to_ref().clone(),f.to_ref().clone());
                stack.push(r);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            27 => { // MD2C
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {} {}","MD2C",f,m,g),pos-1),fmt_stack(&stack)); // 3 args
                let r = call2(m.to_ref().clone(),f.to_ref().clone(),g.to_ref().clone());
                stack.push(r);
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            32|34 => { // VARO|VARU
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                let t = env.ge(x);
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","VARO",x,w),pos-3),fmt_stack(&stack)); // 2 args
                stack.push(Vs::V(t.get(w)));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            33 => { // VARM
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                let t = env.ge(x);
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","VARM",x,w),pos-3),fmt_stack(&stack)); // 2 args
                stack.push(Vs::Slot(t,w));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            48 => { // SETN
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","SETN",i,v),pos-1),fmt_stack(&stack)); // 2 args
                let r = set(true,i,v);
                stack.push(Vs::V(r));
            },
            49 => { // SETU
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","SETU",i,v),pos-1),fmt_stack(&stack)); // 2 args
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            50 => { // SETM
                let i = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {} {}","SETM",i,f,x),pos-1),fmt_stack(&stack)); // 3 args
                let v = call(2,Some(f.to_ref().clone()),Some(x.to_ref().clone()),Some(i.get()));
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
            },
            51 => { // SETC
                let i = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                debug!("{:<22} >> {}",format!("{:<16} @{}",format!("{} {} {}","SETC",i,f),pos-1),fmt_stack(&stack)); // 2 args
                let v = call(1,Some(f.to_ref().clone()),Some(i.get()),None);
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                debug!("{:<22} :  {}","",fmt_stack(&stack));
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
    let builtin = provide();
    prim(r0(builtin).to_array());
    Ok(ok())
}

pub fn run(code: Cc<Code>) -> V {
    let root = Env::new(None,&code.blocks[0],0,None);
    let (pos,_locals) =
        match code.blocks[0].body {
            Body::Imm(b) => code.bodies[b],
            Body::Defer(_,_) => panic!("cant run deferred block"),
        };
    vm(&root,&code,pos,Vec::new()).to_ref().clone()
}

#[should_panic]
pub fn assert_panic(code: Cc<Code>) {
    let _ = run(code);
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Env>,V)> {
    //let code = Code::new(vec![0,0,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,0)]);
    //let root = Env::new(None,&code.blocks[0],None);
    panic!("cant init anything");
    //let rtn = vm(&root,&code,&code.blocks[0],code.blocks[0].pos,Vec::new());
    //Ok((ok(),ResourceArc::new(root),rtn))
}
