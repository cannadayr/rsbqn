use crate::schema::{Env,V,Vs,Vr,Vn,Block,BlockInst,Code,Calleable,Body,A,Ar,Tr2,Tr3,Runtime,Compiler,Prog,set,ok,D2,D1,new_scalar,new_string};
use crate::prim::{provide,decompose,prim_ind};
use crate::code::{r0,r1,c};
use crate::fmt::{dbg_stack_out,dbg_stack_in};
use crate::init_log;
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use cc_mt::Cc;
use std::ops::Deref;
use std::error::Error;
//use std::panic;
use log::{debug, trace, error, log_enabled, info, Level};
use itertools::Itertools;
use num_traits::FromPrimitive;

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
            bl.call_md1(1,D1::new(m.clone(),f))
        },
        V::R1(_,_prim) => Vs::V(V::D1(Cc::new(D1::new(m,f)),None)),
        _ => panic!("call1 with invalid type"),
    }
}
fn call2(m: V,f: V,g: V) -> Vs {
    match m {
        V::BlockInst(ref bl,_prim) => {
            assert_eq!(2,bl.def.typ);
            bl.call_md2(2,D2::new(m.clone(),f,g))
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
            let block_inst = BlockInst::new(env.clone(),block.clone());
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
                    match &x.as_v().unwrap() {
                        V::Nothing => x,
                        _ => call(1,Some(f.into_v().unwrap()),Some(x.into_v().unwrap()),None),
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
                    match (&x.as_v().unwrap(),&w.as_v().unwrap()) {
                        (V::Nothing,_) => x,
                        (_,V::Nothing) => call(1,Some(f.into_v().unwrap()),Some(x.into_v().unwrap()),None),
                        _ => call(2,Some(f.into_v().unwrap()),Some(x.into_v().unwrap()),Some(w.into_v().unwrap()))
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
                    match &f.as_v().unwrap() {
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
                let r = call1(m.into_v().unwrap(),f.into_v().unwrap());
                stack.push(r);
                dbg_stack_out("MD1C",pos-1,&stack);
            },
            27 => { // MD2C
                dbg_stack_in("MD2C",pos-1,"".to_string(),&stack);
                let f = stack.pop().unwrap();
                let m = stack.pop().unwrap();
                let g = stack.pop().unwrap();
                let r = call2(m.into_v().unwrap(),f.into_v().unwrap(),g.into_v().unwrap());
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
                let v = call(2,Some(f.into_v().unwrap()),Some(x.into_v().unwrap()),Some(i.get()));
                let r = set(false,i,v);
                stack.push(Vs::V(r));
                dbg_stack_out("SETM",pos-1,&stack);
            },
            51 => { // SETC
                dbg_stack_in("SETC",pos-1,"".to_string(),&stack);
                let i = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let v = call(1,Some(f.into_v().unwrap()),Some(i.get()),None);
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

pub fn runtime() -> Cc<A> {
    let builtin = provide();
    let runtime0 = r0(&builtin);
    info!("runtime0 loaded");
    match r1(&builtin,runtime0.as_a().unwrap()).into_a().unwrap().get_mut() {
        Some(full_runtime) => {
            let _set_inv = full_runtime.r.pop().unwrap();
            let set_prims = full_runtime.r.pop().unwrap();
            let runtime = full_runtime.r.pop().unwrap();

            // Copy-On-Write. Use two assignments to prevent tmp values freed while in use.
            let mut prims = runtime.into_a().unwrap();
            let mut mut_prims = prims.make_unique();

            // set primitive indices
            for i in 0..mut_prims.r.len()-1 {
                let e = &mut mut_prims.r[i];
                match e {
                    V::UserMd1(_b,_a,ref mut prim) => { *prim = Some(i) },
                    V::UserMd2(_b,_a,ref mut prim) => { *prim = Some(i) },
                    V::BlockInst(_b,ref mut prim) => { *prim = Some(i) },
                    V::Fn(_a,ref mut prim) => { *prim = Some(i) },
                    V::R1(_r1,ref mut prim) => { *prim = Some(i) },
                    V::R2(_r2,ref mut prim) => { *prim = Some(i) },
                    V::D1(_d1,ref mut prim) => { *prim = Some(i) },
                    V::D2(_d2,ref mut prim) => { *prim = Some(i) },
                    V::Tr2(_tr2,ref mut prim) => { *prim = Some(i) },
                    V::Tr3(_tr3,ref mut prim) => { *prim = Some(i) },
                    _ => panic!("illegal setprim"),
                }
            }
            info!("runtime loaded");
            let prim_fns = V::A(Cc::new(A::new(vec![V::Fn(decompose,None),V::Fn(prim_ind,None)],vec![2])));
            let _ = call(1,Some(set_prims),Some(prim_fns),None);
            prims
        },
        None => panic!("cant get mutable runtime"),
    }
}

pub fn prog(compiler: V,src: V,runtime: Cc<A>) -> Cc<Code> {
    let mut prog = call(2,Some(compiler),Some(src),Some(V::A(runtime))).into_v().unwrap().into_a().unwrap();
    info!("prog count = {}",prog.strong_count());
    match prog.get_mut() {
        Some(p) => {
            let _tokenization = p.r.pop().unwrap();
            let _indices      = p.r.pop().unwrap();
            let bodies        = p.r.pop().unwrap();
            let blocks        = p.r.pop().unwrap();
            let objects       = p.r.pop().unwrap();
            let bytecode      = p.r.pop().unwrap();

            Code::new(
                bytecode.as_a().unwrap().r.iter().map(|e| match e {
                    V::Scalar(n) => usize::from_f64(*n).unwrap(),
                    _ => panic!("bytecode not a number"),
                }).collect::<Vec<usize>>(),
                match objects.into_a().unwrap().try_unwrap() {
                    Ok(o) => o.r,
                    Err(_o) => panic!("objects not unique"),
                },
                match blocks.into_a().unwrap().try_unwrap() {
                    Ok(b) => {
                        b.r.iter().map(|e| match e.as_a().unwrap().r.iter().collect_tuple() {
                            Some((V::Scalar(typ),V::Scalar(imm),V::Scalar(body))) =>
                                (
                                    u8::from_f64(*typ).unwrap(),
                                    if 1.0 == *imm { true } else { false },
                                    Body::Imm(usize::from_f64(*body).unwrap())
                                ),
                            Some((V::Scalar(typ),V::Scalar(imm),V::A(bodies))) => {
                                let (mon,dya) = bodies.r.iter().collect_tuple().unwrap();
                                (
                                    u8::from_f64(*typ).unwrap(),
                                    if 1.0 == *imm { true } else { false },
                                    Body::Defer(
                                        mon.as_a().unwrap().r.iter().map(|e| match e {
                                            V::Scalar(n) => usize::from_f64(*n).unwrap(),
                                            _ => panic!("bytecode not a number"),
                                        }).collect::<Vec<usize>>(),
                                        dya.as_a().unwrap().r.iter().map(|e| match e {
                                            V::Scalar(n) => usize::from_f64(*n).unwrap(),
                                            _ => panic!("bytecode not a number"),
                                        }).collect::<Vec<usize>>()
                                    )
                                )
                            },
                            _ => panic!("couldn't load compiled block"),
                        }).collect::<Vec<(u8, bool, Body)>>()
                    },
                    Err(_b) => panic!("cant get unique ref to program blocks"),
                },
                match bodies.into_a().unwrap().try_unwrap() {
                    Ok(b) => {
                        b.r.iter().map(|e| match e.as_a().unwrap().r.iter().collect_tuple() {
                            Some((V::Scalar(pos),V::Scalar(local),_name_id,_export_mask)) =>
                                (usize::from_f64(*pos).unwrap(),usize::from_f64(*local).unwrap()),
                            x => panic!("couldn't load compiled body {:?}",x),
                        }).collect::<Vec<(usize,usize)>>()
                    },
                    Err(_b) => panic!("cant get unique ref to program blocks"),
                }
            )
        },
        None => panic!("cant get unique ref to blocks"),
    }
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

//#[rustler::nif]
//fn init_r() -> NifResult<(Atom,ResourceArc<Runtime>)> {
//    Ok((ok(),ResourceArc::new(Runtime(runtime()))))
//}
#[rustler::nif]
fn init_c(r: ResourceArc<Runtime>) -> NifResult<(Atom,ResourceArc<Compiler>)> {
    let compiler = c(&r.0);
    Ok((ok(),ResourceArc::new(Compiler(compiler.as_block_inst().unwrap().0.clone()))))
}
//#[rustler::nif]
//fn compile(r: ResourceArc<Runtime>,c: ResourceArc<Compiler>,s: &str) -> NifResult<(Atom,ResourceArc<Prog>)> {
//    info!("got src {:?}",&s);
//    let src = new_string(s);
//    let prog = prog(V::BlockInst(c.0.clone(),None),src,r.0.clone());
//    Ok((ok(),ResourceArc::new(Prog(prog.clone()))))
//}
#[rustler::nif]
fn callp(p: ResourceArc<Prog>,n: f64) -> NifResult<(Atom,V)> {
    let result = call(1,Some(run(p.0.clone())),Some(V::Scalar(n)),None);
    Ok((ok(),result.into_v().unwrap()))
}
