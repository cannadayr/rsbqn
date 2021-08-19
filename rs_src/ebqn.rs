use crate::schema::{LateInit,V,Block,Code,Env,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

fn vm(state: &State,code: &Arc<Code>,block: &Arc<Block>,env: Arc<Mutex<Env>>) -> bool {
    true
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<State>)> {
    //[0,0,25],[5],[[0,1,0,0]]
    let code = Code::new(vec![0,0,25],vec![V::Float(5.0)],vec![(0,true,0,0)]);

    let state = State::new();

    //let rtn = vm(&state,&code,&code.blocks[0],env);

    //let mutex = Mutex::new(state);
    //let container = Container { mutex };

    Ok((ok(),ResourceArc::new(state)))
}
