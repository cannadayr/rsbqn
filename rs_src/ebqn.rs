use crate::schema::{Env,V,Vs,Vr,Vn,Block,BlockInst,Code,Calleable,Body,A,Ar,Tr2,Tr3,set,ok,D2,D1};
use crate::prim::{provide,decompose,prim_ind};
use crate::code::{r0,r1};
use crate::fmt::{dbg_stack_out,dbg_stack_in};
use crate::init_log;
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use cc_mt::Cc;
use crate::test::{bytecode,simple,prim};
use std::ops::Deref;
use std::error::Error;
//use std::panic;
use log::{debug, trace, error, log_enabled, info, Level};
use itertools::Itertools;

pub fn call(arity: usize,a: Vn,x: Vn, w: Vn) -> Vs {
    match a {
        Some(v) => v.call(arity,x,w),
        _ => panic!("unimplemented call"),
    }
}
fn call1(m: V,f: V) -> Vs {
    match m {
        V::BlockInst(ref bl,_prim) => {
            assert_eq!(1,bl.def.typ);
            bl.call_block(1,vec![Some(m.clone()),Some(f)])
        },
        V::R1(_,_prim) => Vs::V(V::D1(Cc::new(D1::new(m,f)),None)),
        _ => panic!("call1 with invalid type"),
    }
}
fn call2(m: V,f: V,g: V) -> Vs {
    match m {
        V::BlockInst(ref bl,_prim) => {
            assert_eq!(2,bl.def.typ);
            bl.call_block(2,vec![Some(m.clone()),Some(f),Some(g)])
        },
        V::R2(_,_prim) => Vs::V(V::D2(Cc::new(D2::new(m,f,g)),None)),
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
            let r = Vs::V(V::BlockInst(Cc::new(block_inst),None));
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
    debug!("new eval");
    loop {
        let op = code.bc[pos];pos+=1;
        match op {
            0 => { // PUSH
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                dbg_stack_in("PUSH",pos-2,format!("{} {}",&x,&r),&stack);
                stack.push(Vs::V(r));
                dbg_stack_out("PUSH",pos-2,&stack);
            },
            1 => { // DFND
                let x = code.bc[pos];pos+=1;
                let r = derv(env.clone(),&code,&code.blocks[x]);
                dbg_stack_in("DFND",pos-2,format!("{} {}",&x,&r),&stack);
                stack.push(r);
                dbg_stack_out("DFND",pos-2,&stack);
            },
            6 => { // POPS
                dbg_stack_in("POPS",pos-1,"".to_string(),&stack);
                let _ = stack.pop();
                dbg_stack_out("POPS",pos-1,&stack);
            },
            7 => { // RETN
                break match stack.len() {
                    1 => {
                        dbg_stack_in("RETN",pos-1,"".to_string(),&stack);
                        let rtn = stack.pop().unwrap();
                        rtn
                    },
                    _ => {
                        panic!("stack overflow")
                    }
                };
            },
            11 => { // ARRO
                let x = code.bc[pos];pos+=1;
                dbg_stack_in("ARRO",pos-2,format!("{}",&x),&stack);
                let hd = stack.len() - x;
                let tl = stack.split_off(hd);
                stack.push(list(tl));
                dbg_stack_out("ARRO",pos-2,&stack);
            },
            12 => { // ARRM
                let x = code.bc[pos];pos+=1;
                let hd = stack.len() - x;
                let tl = stack.split_off(hd);
                dbg_stack_in("ARRM",pos-2,format!("{}",&x),&stack);
                stack.push(listr(tl));
                dbg_stack_out("ARRM",pos-2,&stack);
            },
            16|18 => { // FN1C|FN1O
                dbg_stack_in("FN1C",pos-1,"".to_string(),&stack);
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r =
                    match &x.to_ref() {
                        V::Nothing => x,
                        _ => call(1,Some(f.to_ref().clone()),Some(x.to_ref().clone()),None),
                    };
                stack.push(r);
                dbg_stack_out("FN1C",pos-1,&stack);
            },
            17|19 => { // FN2C|FN2O
                dbg_stack_in("FN2C",pos-1,"".to_string(),&stack);
                let w = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r =
                    match (&x.to_ref(),&w.to_ref()) {
                        (V::Nothing,_) => x,
                        (_,V::Nothing) => call(1,Some(f.to_ref().clone()),Some(x.to_ref().clone()),None),
                        _ => call(2,Some(f.to_ref().clone()),Some(x.to_ref().clone()),Some(w.to_ref().clone()))
                    };
                stack.push(r);
                dbg_stack_out("FN2C",pos-1,&stack);
            },
            20 => { // TR2D
                let g = stack.pop().unwrap();
                let h = stack.pop().unwrap();
                dbg_stack_in("TR2D",pos-1,format!("{} {}",&g,&h),&stack);
                let t = Vs::V(V::Tr2(Cc::new(Tr2::new(g,h)),None));
                stack.push(t);
                dbg_stack_out("TR2D",pos-1,&stack);
            },
            21|23 => { // TR3D|TR3O
                dbg_stack_in("TR3D",pos-1,"".to_string(),&stack);
                let f = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                let h = stack.pop().unwrap();
                let t =
                    match &f.to_ref() {
                        V::Nothing => Vs::V(V::Tr2(Cc::new(Tr2::new(g,h)),None)),
                        _ => Vs::V(V::Tr3(Cc::new(Tr3::new(f,g,h)),None)),
                    };
                stack.push(t);
                dbg_stack_out("TR3D",pos-1,&stack);
            },
            26 => { // MD1C
                dbg_stack_in("MD1C",pos-1,"".to_string(),&stack);
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                let r = call1(m.to_ref().clone(),f.to_ref().clone());
                stack.push(r);
                dbg_stack_out("MD1C",pos-1,&stack);
            },
            27 => { // MD2C
                dbg_stack_in("MD2C",pos-1,"".to_string(),&stack);
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                let r = call2(m.to_ref().clone(),f.to_ref().clone(),g.to_ref().clone());
                stack.push(r);
                dbg_stack_out("MD2C",pos-1,&stack);
            },
            32|34 => { // VARO|VARU
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                let t = env.ge(x);
                dbg_stack_in("VARO",pos-3,format!("{} {}",&x,&w),&stack);
                stack.push(Vs::V(t.get(w)));
                dbg_stack_out("VARO",pos-3,&stack);
            },
            33 => { // VARM
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                let t = env.ge(x);
                dbg_stack_in("VARM",pos-3,format!("{} {}",&x,&w),&stack);
                stack.push(Vs::Slot(t,w));
                dbg_stack_out("VARM",pos-3,&stack);
            },
            48 => { // SETN
                dbg_stack_in("SETN",pos-1,"".to_string(),&stack);
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(true,i,v);
                stack.push(Vs::V(r));
                dbg_stack_out("SETN",pos-1,&stack);
            },
            49 => { // SETU
                dbg_stack_in("SETU",pos-1,"".to_string(),&stack);
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                dbg_stack_out("SETU",pos-1,&stack);
            },
            50 => { // SETM
                dbg_stack_in("SETM",pos-1,"".to_string(),&stack);
                let i = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let v = call(2,Some(f.to_ref().clone()),Some(x.to_ref().clone()),Some(i.get()));
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                dbg_stack_out("SETM",pos-1,&stack);
            },
            51 => { // SETC
                dbg_stack_in("SETC",pos-1,"".to_string(),&stack);
                let i = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let v = call(1,Some(f.to_ref().clone()),Some(i.get()),None);
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                dbg_stack_out("SETC",pos-1,&stack);
            },
            _ => {
                panic!("unreachable op: {}",op);
            }
        }
    }
}

#[test]
fn test() -> Result<(),Box<std::error::Error>> {
    init_log();
    //bytecode();
    //info!("bytecode loaded");
    let builtin = provide();
    let runtime0 = r0(&builtin);
    info!("runtime0 loaded");
    let full_runtime = r1(&builtin,runtime0.as_a().unwrap()).into_a().unwrap().try_unwrap().unwrap();
    let (runtime_w,set_prims,set_inv_w) = full_runtime.r.iter().collect_tuple().unwrap();
    // iterate thru this and create a new runtime array with set primitive indices
    let runtime = A::new((*runtime_w).as_a().unwrap().r.iter().enumerate().map(|(i,e)| match e {
        V::DervBlockInst(b,a,_prim) => V::DervBlockInst(b.clone(),a.clone(),Some(i)),
        V::BlockInst(b,_prim) => V::BlockInst(b.clone(),Some(i)),
        V::Fn(a,_prim) => V::Fn(a.clone(),Some(i)),
        V::R1(r1,_prim) => V::R1(r1.clone(),Some(i)),
        V::R2(r2,_prim) => V::R2(r2.clone(),Some(i)),
        V::D1(d1,_prim) => V::D1(d1.clone(),Some(i)),
        V::D2(d2,_prim) => V::D2(d2.clone(),Some(i)),
        V::Tr2(tr2,_prim) => V::Tr2(tr2.clone(),Some(i)),
        V::Tr3(tr3,_prim) => V::Tr3(tr3.clone(),Some(i)),
        _ => panic!("illegal setprim"),
    }).collect::<Vec<V>>(),vec![64]);
    let set_inv = (*set_inv_w).as_block_inst().unwrap();
    info!("runtime loaded");
    let prim_fns = V::A(Cc::new(A::new(vec![V::Fn(decompose,None),V::Fn(prim_ind,None)],vec![2])));
    let _ = call(1,Some(set_prims.clone()),Some(prim_fns),None);
    info!("indices loaded");
    Ok(())
}

pub fn run(code: Cc<Code>) -> V {
    let root = Env::new(None,&code.blocks[0],0,None);
    let (pos,_locals) =
        match code.blocks[0].body {
            Body::Imm(b) => code.bodies[b],
            Body::Defer(_,_) => panic!("cant run deferred block"),
        };
    vm(&root,&code,pos,Vec::new()).into_v().unwrap()
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Env>,V)> {
    //let code = Code::new(vec![0,0,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,0)]);
    //let root = Env::new(None,&code.blocks[0],None);
    panic!("cant init anything");
    //let rtn = vm(&root,&code,&code.blocks[0],code.blocks[0].pos,Vec::new());
    //Ok((ok(),ResourceArc::new(root),rtn))
}
