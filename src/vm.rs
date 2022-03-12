use crate::schema::{Env,V,Vs,Vn,Ve,Block,BlockInst,Code,Calleable,Stacker,Stack,Bodies,Exp,A,Ar,Tr2,Tr3,Runtime,Compiler,Prog,D2,D1,Fn,new_scalar,new_string};
use crate::provide::{provide,decompose,prim_ind,typ,glyph,fmtnum};
use crate::gen::code::{r0,r1,c,f};
use crate::fmt::{dbg_stack_out,dbg_stack_in};
use crate::init_log;
use bacon_rajan_cc::Cc;
use std::ops::Deref;
use std::error::Error;
use std::collections::VecDeque;
use std::ptr;
//use std::panic;
use log::{debug, trace, error, log_enabled, info, Level};
use itertools::Itertools;
use num_traits::FromPrimitive;

pub fn call(stack: &mut Stack,arity: usize,a: Vn,x: Vn, w: Vn) -> Result<Vs,Ve> {
    match a.0 {
        Some(v) => v.call(stack,arity,x,w),
        _ => panic!("unimplemented call"),
    }
}
fn call1(stack: &mut Stack,m: V,f: V) -> Result<Vs,Ve> {
    match m {
        V::BlockInst(ref bl,_prim) => {
            assert_eq!(1,bl.def.typ);
            bl.call_md1(stack,1,D1::new(m.clone(),f))
        },
        V::R1(_,_prim) => Ok(Vs::V(V::D1(Cc::new(D1::new(m,f)),None))),
        _ => panic!("call1 with invalid type"),
    }
}
fn call2(stack: &mut Stack,m: V,f: V,g: V) -> Result<Vs,Ve> {
    match m {
        V::BlockInst(ref bl,_prim) => {
            assert_eq!(2,bl.def.typ);
            bl.call_md2(stack,2,D2::new(m.clone(),f,g))
        },
        V::R2(_,_prim) => Ok(Vs::V(V::D2(Cc::new(D2::new(m,f,g)),None))),
        _ => panic!("call2 with invalid type"),
    }
}

fn derv(env: &Env,code: &Cc<Code>,block: &Cc<Block>,stack: &mut Stack) -> Result<Vs,Ve> {
    match (block.typ,block.imm) {
        (0,true) => {
            let child = Env::new(Some(env),block,0,None);
            let pos = match block.bodies {
                Bodies::Comp(b) => {
                    let (p,_l) = code.body_ids[b];
                    p
                },
                _ => panic!("body immediacy derivation doesnt match block definition"),
            };
            vm(&child,code,None,None,pos,stack)
        },
        (_typ,_imm) => {
            let block_inst = BlockInst::new(env.clone(),block.clone());
            let r = Vs::V(V::BlockInst(Cc::new(block_inst),None));
            Ok(r)
        },
    }
}

fn list(ravel: Vec<V>) -> Vs {
    let shape = vec![ravel.len() as usize];
    Vs::V(V::A(Cc::new(A::new(ravel,shape))))
}

fn incr(stack: &mut Stack) {
    stack.fp = stack.s.len();
}

pub fn vm(env: &Env,code: &Cc<Code>,bodies: Option<&Vec<usize>>,body_id: Option<usize>,mut pos: usize,mut stack: &mut Stack) -> Result<Vs,Ve>  {
    #[cfg(feature = "debug")]
    incr(stack);
    #[cfg(feature = "debug")]
    debug!("new eval");
    loop {
        // we are making the following assumptions:
        //  1. the BQN compiler is producing correct output
        //  2. the BQN virtual machine is compatible with the loaded compiler
        // using an unsafe function because this loop is performance critical
        match unsafe { code.bc.get_unchecked(pos) } {
            0 => { // PUSH
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("PUSH");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let r = code.objs[x].clone();
                #[cfg(feature = "debug")]
                dbg_stack_in("PUSH",pos-2,format!("{} {}",&x,&r),stack);
                stack.s.push_unchecked(Vs::V(r));
                #[cfg(feature = "debug")]
                dbg_stack_out("PUSH",pos-2,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("PUSH");
            },
            1 => { // DFND
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("DFND");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let r =
                match derv(&env,&code,&code.blocks[x],&mut stack) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                #[cfg(feature = "debug")]
                dbg_stack_in("DFND",pos-2,format!("{} {}",&x,&r),stack);
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("DFND",pos-2,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("DFND");
            },
            6 => { // POPS
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("POPS");
                #[cfg(feature = "debug")]
                dbg_stack_in("POPS",pos-1,"".to_string(),stack);
                let _ = stack.s.pop_unchecked();
                #[cfg(feature = "debug")]
                dbg_stack_out("POPS",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("POPS");
            },
            7 => { // RETN
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("RETN");
                #[cfg(feature = "debug")]
                dbg_stack_in("RETN",pos-1,"".to_string(),stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("RETN");
                break Ok(stack.s.pop_unchecked());
            },
            11 => { // ARRO
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("ARRO");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                #[cfg(feature = "debug")]
                dbg_stack_in("ARRO",pos-2,format!("{}",&x),stack);
                let v = stack.s.pop_list_unchecked(x);
                stack.s.push_unchecked(list(v));
                #[cfg(feature = "debug")]
                dbg_stack_out("ARRO",pos-2,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("ARRO");
            },
            12 => { // ARRM
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("ARRM");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                #[cfg(feature = "debug")]
                dbg_stack_in("ARRM",pos-2,format!("{}",&x),stack);
                let v = stack.s.pop_ref_list_unchecked(x);
                stack.s.push_unchecked(Vs::Ar(Ar::new(v)));
                #[cfg(feature = "debug")]
                dbg_stack_out("ARRM",pos-2,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("ARRM");
            },
            16 => { // FN1C
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("FN1C");
                #[cfg(feature = "debug")]
                dbg_stack_in("FN1C",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let x = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                let r =
                match call(&mut stack,1,Vn(Some(&f.into_v().unwrap())),Vn(Some(&x.into_v().unwrap())),Vn(None)) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("FN1C",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("FN1C");
            },
            18 => { // FN1O
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("FN1O");
                #[cfg(feature = "debug")]
                dbg_stack_in("FN1O",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let x = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                let r =
                    match &x.as_v().unwrap() {
                        V::Nothing => x,
                        _ => match call(&mut stack,1,Vn(Some(&f.into_v().unwrap())),Vn(Some(&x.into_v().unwrap())),Vn(None)) {
                            Ok(r) => r,
                            Err(e) => break Err(e),
                        },
                    };
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("FN1O",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("FN1O");
            },
            17 => { // FN2C
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("FN2C");
                #[cfg(feature = "debug")]
                dbg_stack_in("FN2C",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let w = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                let x = unsafe { ptr::read(stack.s.as_ptr().add(l-3)) };
                unsafe { stack.s.set_len(l-3) };
                let r =
                match call(&mut stack,2,Vn(Some(&f.into_v().unwrap())),Vn(Some(&x.into_v().unwrap())),Vn(Some(&w.into_v().unwrap()))) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("FN2C",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("FN2C");
            },
            19 => { // FN2O
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("FN2O");
                #[cfg(feature = "debug")]
                dbg_stack_in("FN2O",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let w = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                let x = unsafe { ptr::read(stack.s.as_ptr().add(l-3)) };
                unsafe { stack.s.set_len(l-3) };
                let r =
                    match (&x.as_v().unwrap(),&w.as_v().unwrap()) {
                        (V::Nothing,_) => x,
                        (_,V::Nothing) => match call(&mut stack,1,Vn(Some(&f.into_v().unwrap())),Vn(Some(&x.into_v().unwrap())),Vn(None)) {
                            Ok(r) => r,
                            Err(e) => break Err(e),
                        },
                        _ => match call(&mut stack,2,Vn(Some(&f.into_v().unwrap())),Vn(Some(&x.into_v().unwrap())),Vn(Some(&w.into_v().unwrap()))) {
                            Ok(r) => r,
                            Err(e) => break Err(e),
                        },
                    };
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("FN2O",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("FN2O");
            },
            20 => { // TR2D
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("TR2D");
                let l = stack.s.len();
                let g = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let h = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                #[cfg(feature = "debug")]
                dbg_stack_in("TR2D",pos-1,format!("{} {}",&g,&h),stack);
                let t = Vs::V(V::Tr2(Cc::new(Tr2::new(g,h)),None));
                stack.s.push_unchecked(t);
                #[cfg(feature = "debug")]
                dbg_stack_out("TR2D",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("TR2D");
            },
            21 => { // TR3D
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("TR3D");
                #[cfg(feature = "debug")]
                dbg_stack_in("TR3D",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let g = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                let h = unsafe { ptr::read(stack.s.as_ptr().add(l-3)) };
                unsafe { stack.s.set_len(l-3) };
                let t = Vs::V(V::Tr3(Cc::new(Tr3::new(f,g,h)),None));
                stack.s.push_unchecked(t);
                #[cfg(feature = "debug")]
                dbg_stack_out("TR3D",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("TR3D");
            },
            23 => { // TR3O
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("TR3O");
                #[cfg(feature = "debug")]
                dbg_stack_in("TR3O",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let g = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                let h = unsafe { ptr::read(stack.s.as_ptr().add(l-3)) };
                unsafe { stack.s.set_len(l-3) };
                let t =
                    match &f.as_v().unwrap() {
                        V::Nothing => Vs::V(V::Tr2(Cc::new(Tr2::new(g,h)),None)),
                        _ => Vs::V(V::Tr3(Cc::new(Tr3::new(f,g,h)),None)),
                    };
                stack.s.push_unchecked(t);
                #[cfg(feature = "debug")]
                dbg_stack_out("TR3O",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("TR3O");
            },
            26 => { // MD1C
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("MD1C");
                #[cfg(feature = "debug")]
                dbg_stack_in("MD1C",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let m = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                let r = match call1(&mut stack,m.into_v().unwrap(),f.into_v().unwrap()) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("MD1C",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("MD1C");
            },
            27 => { // MD2C
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("MD2C");
                #[cfg(feature = "debug")]
                dbg_stack_in("MD2C",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let m = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                let g = unsafe { ptr::read(stack.s.as_ptr().add(l-3)) };
                unsafe { stack.s.set_len(l-3) };
                let r = match call2(&mut stack,m.into_v().unwrap(),f.into_v().unwrap(),g.into_v().unwrap()) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                stack.s.push_unchecked(r);
                #[cfg(feature = "debug")]
                dbg_stack_out("MD2C",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("MD2C");
            },
            32 => { // VARO
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("VARO");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let w = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let t = env.ge(x);
                #[cfg(feature = "debug")]
                dbg_stack_in("VARO",pos-3,format!("{} {}",&x,&w),stack);
                stack.s.push_unchecked(Vs::V(t.get(w)));
                #[cfg(feature = "debug")]
                dbg_stack_out("VARO",pos-3,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("VARO");
            },
            34 => { // VARU
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("VARU");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let w = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let t = env.ge(x);
                #[cfg(feature = "debug")]
                dbg_stack_in("VARU",pos-3,format!("{} {}",&x,&w),stack);
                stack.s.push_unchecked(Vs::V(t.get_drop(w)));
                #[cfg(feature = "debug")]
                dbg_stack_out("VARU",pos-3,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("VARU");
            },
            33 => { // VARM
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("VARM");
                let x = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let w = unsafe { *code.bc.get_unchecked(pos) };pos+=1;
                let t = env.ge(x);
                #[cfg(feature = "debug")]
                dbg_stack_in("VARM",pos-3,format!("{} {}",&x,&w),stack);
                stack.s.push_unchecked(Vs::Slot(t.clone(),w));
                #[cfg(feature = "debug")]
                dbg_stack_out("VARM",pos-3,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("VARM");
            },
            43 => { // VFYM
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("VFYM");
                let m = stack.s.pop_unchecked();
                #[cfg(feature = "debug")]
                dbg_stack_in("VFYM",pos-1,"".to_string(),stack);
                stack.s.push_unchecked(Vs::Match(Some(m.into_v().unwrap())));
                #[cfg(feature = "debug")]
                dbg_stack_out("VFYM",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("VFYM");
            },
            44 => { // NOTM
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("NOTM");
                #[cfg(feature = "debug")]
                dbg_stack_in("NOTM",pos-1,"".to_string(),stack);
                stack.s.push_unchecked(Vs::Match(None));
                #[cfg(feature = "debug")]
                dbg_stack_out("NOTM",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("NOTM");
            },
            47 => { // SETH
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("SETH");
                #[cfg(feature = "debug")]
                dbg_stack_in("SETH",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let i = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let v = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                match i.set(true,v.as_v().unwrap()) {
                    Ok(r) => (), // continue
                    Err(_) => {
                        // move to next body in list
                        match (bodies,body_id) {
                            (Some(b),Some(id)) => {
                                let (p,locals) = unsafe { code.body_ids.get_unchecked(b[id+1]) };
                                break vm(&env.reinit(*locals),&code,Some(b),Some(id+1),*p,stack);
                            }
                            _ => panic!("no successive body in SETH"),
                        };
                    },
                }
                #[cfg(feature = "debug")]
                dbg_stack_out("SETH",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("SETH");
            },
            48 => { // SETN
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("SETN");
                #[cfg(feature = "debug")]
                dbg_stack_in("SETN",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let i = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let v = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                let r = i.set(true,v.as_v().unwrap())?;
                stack.s.push_unchecked(Vs::V(r));
                #[cfg(feature = "debug")]
                dbg_stack_out("SETN",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("SETN");
            },
            49 => { // SETU
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("SETU");
                #[cfg(feature = "debug")]
                dbg_stack_in("SETU",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let i = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let v = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                let r = i.set(false,v.as_v().unwrap())?;
                stack.s.push_unchecked(Vs::V(r));
                #[cfg(feature = "debug")]
                dbg_stack_out("SETU",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("SETU");
            },
            50 => { // SETM
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("SETM");
                #[cfg(feature = "debug")]
                dbg_stack_in("SETM",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let i = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                let x = unsafe { ptr::read(stack.s.as_ptr().add(l-3)) };
                unsafe { stack.s.set_len(l-3) };
                let v = match call(&mut stack,2,Vn(Some(&f.into_v().unwrap())),Vn(Some(&x.into_v().unwrap())),Vn(Some(&i.get()))) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                let r = i.set(false,v.as_v().unwrap())?;
                stack.s.push_unchecked(Vs::V(r));
                #[cfg(feature = "debug")]
                dbg_stack_out("SETM",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("SETM");
            },
            51 => { // SETC
                pos += 1;
                #[cfg(feature = "coz-ops")]
                coz::begin!("SETC");
                #[cfg(feature = "debug")]
                dbg_stack_in("SETC",pos-1,"".to_string(),stack);
                let l = stack.s.len();
                let i = unsafe { ptr::read(stack.s.as_ptr().add(l-1)) };
                let f = unsafe { ptr::read(stack.s.as_ptr().add(l-2)) };
                unsafe { stack.s.set_len(l-2) };
                let v = match call(&mut stack,1,Vn(Some(&f.into_v().unwrap())),Vn(Some(&i.get())),Vn(None)) {
                    Ok(r) => r,
                    Err(e) => break Err(e),
                };
                let r = i.set(false,v.as_v().unwrap())?;
                stack.s.push_unchecked(Vs::V(r));
                #[cfg(feature = "debug")]
                dbg_stack_out("SETC",pos-1,stack);
                #[cfg(feature = "coz-ops")]
                coz::end!("SETC");
            },
            _ => {
                panic!("unreachable op: {}",code.bc[pos]);
            }
        }
        #[cfg(feature = "coz-loop")]
        coz::progress!();
    }
}

pub fn runtime(root: Option<&Env>,stack: &mut Stack) -> Result<V,Ve> {
    let builtin = provide();
    let runtime0 = run(root,stack,r0(&builtin))?;
    let runtime1 = run(root,stack,r1(&builtin,&runtime0))?;
    info!("runtime0 loaded");
    match runtime1.into_a().unwrap().get_mut() {
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
            let prim_fns = V::A(Cc::new(A::new(vec![V::Fn(Fn(decompose),None),V::Fn(Fn(prim_ind),None)],vec![2])));
            let _ = call(stack,1,Vn(Some(&set_prims)),Vn(Some(&prim_fns)),Vn(None));
            Ok(V::A(prims))
        },
        None => panic!("cant get mutable runtime"),
    }
}

pub fn sysfns(arity: usize,x: Vn,w:Vn) -> Result<Vs,Ve> {
    Ok(Vs::V(V::A(Cc::new(A::new(vec![],vec![0])))))
}

pub fn prog(stack: &mut Stack,compiler: &V,src: V,runtime: &V,env: &Env,names: &V,redef: &V,strictness: f64) -> Result<(Cc<Code>,A,A),Ve> {
    // an array ravel is a vector of owned values
    // because we are passing the vars/names/redefs as elements in an array, they must be moved to the new prog
    // this will likely result in excess clones
    let vars = env.to_vars();
    let args = V::A(Cc::new(A::new(vec![runtime.clone(),V::Fn(Fn(sysfns),None),names.clone(),redef.clone()],vec![4])));
    let mut prog = call(stack,2,Vn(Some(compiler)),Vn(Some(&src)),Vn(Some(&args)))?.into_v().unwrap().into_a().unwrap();
    match prog.get_mut() {
        Some(p) => {
            let tokenization = p.r.pop().unwrap();
            let _indices      = p.r.pop().unwrap();
            let bodies        = p.r.pop().unwrap();
            let blocks        = p.r.pop().unwrap();
            let objects       = p.r.pop().unwrap();
            let bytecode      = p.r.pop().unwrap();

            // repl stuff
            let varlen = vars.as_a().unwrap().r.len();
            //info!("varlen = {}",&varlen);
            let pnames = &tokenization.as_a().unwrap().r[2].as_a().unwrap().r[0].as_a().unwrap().r;
            //info!("pnames = {:?}",&pnames);
            let newv = &bodies.as_a().unwrap().r[0].as_a().unwrap().r[2].as_a().unwrap().r[varlen..];
            //info!("newv = {:?}",newv);

            let mut namestmp = names.as_a().unwrap().clone();
            let namesmut = namestmp.make_unique();
            let mut newnames = newv.iter().map(|i| pnames[usize::from_f64(*i.as_scalar().unwrap()).unwrap()].clone()).collect::<Vec<V>>();
            namesmut.r.append(&mut newnames);
            namesmut.sh = vec![namesmut.r.len()];

            let mut redeftmp = redef.as_a().unwrap().clone();
            let redefmut = redeftmp.make_unique();
            let mut newredef = newv.iter().map(|_i| V::Scalar(strictness)).collect::<Vec<V>>();
            redefmut.r.append(&mut newredef);
            redefmut.sh = vec![redefmut.r.len()];

            // extend root env
            env.extend(newv.len());
            // end repl stuff

            let bc = bytecode.as_a().unwrap().r.iter().map(|e| match e {
                V::Scalar(n) => usize::from_f64(*n).unwrap(),
                _ => panic!("bytecode not a number"),
            }).collect::<Vec<usize>>();
            let objs = match objects.into_a().unwrap().try_unwrap() {
                Ok(o) => o.r,
                Err(_o) => panic!("objects not unique"),
            };
            let blocks_raw = match blocks.into_a().unwrap().try_unwrap() {
                Ok(b) => {
                    b.r.iter().map(|e| match e.as_a().unwrap().r.iter().collect_tuple() {
                        Some((V::Scalar(typ),V::Scalar(imm),V::Scalar(body))) =>
                            (
                                u8::from_f64(*typ).unwrap(),
                                if 1.0 == *imm { true } else { false },
                                Bodies::Comp(usize::from_f64(*body).unwrap())
                            ),
                        Some((V::Scalar(typ),V::Scalar(imm),V::A(bodies))) => {
                            let (mon,dya) = bodies.r.iter().collect_tuple().unwrap();
                            (
                                u8::from_f64(*typ).unwrap(),
                                if 1.0 == *imm { true } else { false },
                                Bodies::Exp(Exp(
                                    mon.as_a().unwrap().r.iter().map(|e| match e {
                                        V::Scalar(n) => usize::from_f64(*n).unwrap(),
                                        _ => panic!("bytecode not a number"),
                                    }).collect::<Vec<usize>>(),
                                    dya.as_a().unwrap().r.iter().map(|e| match e {
                                        V::Scalar(n) => usize::from_f64(*n).unwrap(),
                                        _ => panic!("bytecode not a number"),
                                    }).collect::<Vec<usize>>()
                                ))
                            )
                        },
                        _ => panic!("couldn't load compiled block"),
                    }).collect::<Vec<(u8, bool, Bodies)>>()
                },
                Err(_b) => panic!("cant get unique ref to program blocks"),
            };
            let bods = match bodies.into_a().unwrap().try_unwrap() {
                Ok(b) => {
                    b.r.iter().map(|e| match e.as_a().unwrap().r.iter().collect_tuple() {
                        Some((V::Scalar(pos),V::Scalar(local),_name_id,_export_mask)) =>
                            (usize::from_f64(*pos).unwrap(),usize::from_f64(*local).unwrap()),
                        _x => panic!("couldn't load compiled body"),
                    }).collect::<Vec<(usize,usize)>>()
                },
                Err(_b) => panic!("cant get unique ref to program blocks"),
            };
            Ok((Code::new(bc,objs,blocks_raw,bods),namesmut.to_owned(),redefmut.to_owned()))
        },
        None => panic!("cant get unique ref to blocks"),
    }
}

pub fn formatter(root: Option<&Env>,stack: &mut Stack,runtime: &V) -> Result<V,Ve> {
    let formatter = run(root,stack,f(&runtime)).expect("couldnt load fmt");
    let fmt_fns = V::A(Cc::new(A::new(vec![V::Fn(Fn(typ),None),V::Fn(Fn(decompose),None),V::Fn(Fn(glyph),None),V::Fn(Fn(fmtnum),None)],vec![4])));
    let mut fmt = call(stack,1,Vn(Some(&formatter)),Vn(Some(&fmt_fns)),Vn(None)).expect("fmt malformed").into_v().unwrap().into_a().unwrap();
    let mut fmt1 = fmt.make_unique();
    let repr = fmt1.r.pop().unwrap();
    let fmt1 = fmt1.r.pop().unwrap();
    Ok(fmt1)
}

pub fn run_in_place(env: &Env,stack: &mut Stack,code: Cc<Code>) -> Result<V,Ve> {
    let (pos,_locals) =
        match code.blocks[0].bodies {
            Bodies::Comp(b) => code.body_ids[b],
            Bodies::Exp(Exp(_,_)) => panic!("cant run deferred block"),
        };
    match vm(&env,&code,None,None,pos,stack) {
        Ok(r) => Ok(r.into_v().unwrap()),
        Err(e) => Err(e),
    }
}
pub fn run(parent: Option<&Env>,stack: &mut Stack,code: Cc<Code>) -> Result<V,Ve> {
    let child = Env::new(parent,&code.blocks[0],0,None);
    run_in_place(&child,stack,code)
}
