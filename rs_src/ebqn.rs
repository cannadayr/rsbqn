use crate::schema::{LateInit,EnvRef,V,Vs,Block,Code,Env,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};

fn vm(state: &State,code: &Arc<Code>,block: &Arc<Block>,env: EnvRef,mut pos: usize,mut stack: Vec<Vs>) -> Vs {
    debug!("block (typ,imm,locals,pos) : ({},{},{},{})",block.typ,block.imm,block.locals,block.pos);
    loop {
        let op = code.bc[pos];pos+=1;
        match op {
            0 => {
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                stack.push(Vs::Ref(r))
            },
            // combine 11 & 12 for now
            11|12 => {
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = i.set(true,v); // rtns a reference to v
                stack.push(Vs::Ref(r));
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
                        0 => Vs::Slot(env.clone(),w),
                        _ => panic!("ge not implemented")
                    };
                stack.push(t)
            },
            25 => {
                break match stack.len() {
                    1 => {
                        stack.pop().unwrap()
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
fn init_st() -> NifResult<(Atom,ResourceArc<State>,Vs)> {
    //[0,0,25],[5],[[0,1,0,0]]
    //let code = Code::new(vec![0,0,25],vec![Cc::new(V::Scalar(5.0))],vec![(0,true,0,0)]); // 5
    //let code = Code::new(vec![0,0,14,0,1,25],vec![Cc::new(V::Scalar(4.0)),Cc::new(V::Scalar(3.0))],vec![(0,true,0,0)]); // 3
    let code = Code::new(vec![0,0,22,0,0,11,25],vec![Cc::new(V::Scalar(5.0))],vec![(0,true,1,0)]); // 5

    let state = State::new(&code.blocks[0]);

    let rtn = vm(&state,&code,&code.blocks[0],state.root.clone(),code.blocks[0].pos,Vec::new());

    Ok((ok(),ResourceArc::new(state),rtn))
}
