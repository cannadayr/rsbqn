use crate::schema::{Env,Vu,Vs,Vn,Block,Code,State,set,new_scalar,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Arc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};

fn ge(env: Env,i: usize) -> Env {
    match i {
        0 => env,
        _ => panic!("ge not implemented for i > 0")
    }
}

fn call(a: Vn,x: Vn, w: Vn) -> Vs {
    debug!("(a,x,w):({:?},{:?},{:?})",a,x,w);
    match a {
        Some(v) => {
            debug!("v is {:?}",v);
            match *v {
                Vu::Scalar(n) => Vs::Ref(v),
                _ => panic!("unrecognized call arg"),
            }
        },
        _ => panic!("unimplemented call"),
    }
}

fn vm(state: &State,code: &Arc<Code>,block: &Arc<Block>,env: Env,mut pos: usize,mut stack: Vec<Vs>) -> Vs {
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
                let r = set(true,i,v); // rtns a reference to v
                stack.push(Vs::Ref(r));
            },
            14 => {
                let _ = stack.pop();
            },
            16 => {
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(Some(f.to_ref().clone()),Some(x.to_ref().clone()),None);
                stack.push(r);
            },
            21 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 21 (x,w):({},{})",x,w);
                let t = ge(env.clone(),x);
                stack.push(Vs::Ref(t.get(w)))
            },
            22 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 22 (x,w):({},{})",x,w);
                let t = ge(env.clone(),x);
                stack.push(Vs::Slot(t,w))
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
    // remember to swap last 2 block attrs from erlang version
    //let code = Code::new(vec![],vec![],vec![]);

    //let code = Code::new(vec![0,0,25],vec![new_scalar(5.0)],vec![(0,true,0,0)]); // 5
    //let code = Code::new(vec![0,0,14,0,1,25],vec![new_scalar(4.0),new_scalar(3.0)],vec![(0,true,0,0)]); // 3
    //let code = Code::new(vec![0,0,22,0,0,11,25],vec![new_scalar(5.0)],vec![(0,true,1,0)]); // 5
    //let code = Code::new(vec![0,0,22,0,0,11,14,0,1,22,0,0,12,25],vec![new_scalar(5.0),new_scalar(4.0)],vec![(0,true,1,0)]); // 4
    //let code = Code::new(vec![0,0,22,0,0,11,14,0,1,22,0,1,11,14,21,0,0,25],vec![new_scalar(2.0),new_scalar(3.0)],vec![(0,true,2,0)]); // 2
    let code = Code::new(vec![0,0,22,0,0,11,14,0,1,21,0,0,16,25],vec![new_scalar(1.0),new_scalar(4.0)],vec![(0,true,1,0)]);

    let state = State::new(&code.blocks[0]);

    let rtn = vm(&state,&code,&code.blocks[0],state.root.clone(),code.blocks[0].pos,Vec::new());

    Ok((ok(),ResourceArc::new(state),rtn))
}
