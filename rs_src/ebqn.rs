use crate::schema::{LateInit,V,Block,Code,Env,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};

fn vm(state: &State,code: &Arc<Code>,block: &Arc<Block>,env: Cc<Mutex<Env>>,mut pos: usize,mut stack: Vec<Cc<V>>) -> bool {
    debug!("block (typ,imm,locals,pos) : ({},{},{},{})",block.typ,block.imm,block.locals,block.pos);
    let op = code.bc[pos];
    debug!("pos : {}",pos);
    loop {
        let op = code.bc[pos];pos+=1;
        match op {
            0 => {
                let x = code.bc[pos];pos+=1;
                //let r = V::Cc(Cc::new(code.objs[x]));
                let r = code.objs[x].clone();
                stack.push(r.clone())
            }
            _ => {
                error!("unreachable op");
                panic!();
            }
        }
        debug!("op : {}",op);
    }

    true
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<State>)> {
    //[0,0,25],[5],[[0,1,0,0]]
    let code = Code::new(vec![0,0,25],&vec![V::Float(5.0)],vec![(0,true,0,0)]);

    let state = State::new();

    let rtn = vm(&state,&code,&code.blocks[0],state.root.clone(),code.blocks[0].pos,Vec::new());

    Ok((ok(),ResourceArc::new(state)))
}
