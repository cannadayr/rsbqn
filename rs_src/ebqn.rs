use crate::schema::{LateInit,V,Block,Code,Env,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container<'static>>)> {
    //[0,0,25],[5],[[0,1,0,0]]
    let code = Code::new(vec![0,0,25],vec![V::Float(5.0)],vec![(0,true,0,0)]);

    let mut state = State::new();
    let root_env = Arc::new(Env {vars: Vec::new(), ..Env::default()});
    state.alloc(root_env);

    let mutex = Mutex::new(state);
    let container = Container { mutex };

    Ok((ok(),ResourceArc::new(container)))
}
