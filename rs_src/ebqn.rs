use crate::schema::{LateInit,V,Block,Code,Env,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};

fn set(d: bool,i: V, v: V) -> Cc<V> {
    debug!("type is {:?}",i);
    match i {
        V::Slot(arc) => {
            let mut guard = arc.lock().unwrap();
            let t = Cc::new(v);
            let r = t.clone();
            *guard = Some(V::Obj(t));
            drop(guard);
            r
        },
        _ => panic!("array case not implemented"),
    }
}
fn vm(state: &State,code: &Arc<Code>,block: &Arc<Block>,env: Cc<Mutex<Env>>,mut pos: usize,mut stack: Vec<V>) -> Cc<V> {
    debug!("block (typ,imm,locals,pos) : ({},{},{},{})",block.typ,block.imm,block.locals,block.pos);
    loop {
        let op = code.bc[pos];pos+=1;
        match op {
            0 => {
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                stack.push(V::Obj(r))
            },
            // combine 11 & 12 for now
            11|12 => {
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(true,i,v); // rtns a reference to v
                stack.push(V::Obj(r));
            },
            14 => {
                let _ = stack.pop();
            },
            22 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 22 (x,w) : ({},{})",x,w);
                let t =
                    match x {
                        0 => {
                            let mut env_guard = env.lock().unwrap();
                            let slot = env_guard.vars[w].clone();
                            drop(env_guard);
                            slot
                        },
                        _ => panic!("ge not implemented")
                    };
                stack.push(V::Slot(t))
            },
            25 => {
                break match stack.len() {
                    1 => {
                        Cc::new(stack.pop().unwrap())
                    },
                    _ => {
                        panic!("stack overflow")
                    }
                };
            },
            _ => {
                panic!("unreachable op: {}",op);
            }
        }
        debug!("op : {}",op);
    }
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<State>,V)> {
    //[0,0,25],[5],[[0,1,0,0]]
    //let code = Code::new(vec![0,0,25],vec![Cc::new(V::Scalar(5.0))],vec![(0,true,0,0)]); // 5
    //let code = Code::new(vec![0,0,14,0,1,25],vec![Cc::new(V::Scalar(4.0)),Cc::new(V::Scalar(3.0))],vec![(0,true,0,0)]); // 3
    let code = Code::new(vec![0,0,22,0,0,11,25],vec![Cc::new(V::Scalar(5.0))],vec![(0,true,1,0)]); // 5

    let state = State::new(&code.blocks[0]);

    let arc = vm(&state,&code,&code.blocks[0],state.root.clone(),code.blocks[0].pos,Vec::new());

    let rtn =
        match arc.try_unwrap() {
            Ok(slot) => {
                slot
            },
            r => panic!("cant match rtn")
        };

    Ok((ok(),ResourceArc::new(state),rtn))
}
