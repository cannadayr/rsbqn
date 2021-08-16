use crate::schema::{LateInit,V,Block,Code,Env,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container<'static>>)> {
    //[0,0,25],[5],[[0,1,0,0]]
    let code  = Arc::new(Code {bc: vec![0,0,25], objs: vec![V::Float(5.0)], ..Code::default()});
    let block = Arc::new(Block {typ: 0, imm: true, locals: 0, pos: 0, ..Block::default()});
    block.code.init(&code);
    let blocks = vec![block];
    code.blocks.init(&blocks);

    let mut state = State::new();
    let root_env = Arc::new(Env {vars: Vec::new(), ..Env::default()});
    state.alloc(root_env);

    let mutex = Mutex::new(state);
    let container = Container { mutex };

    Ok((ok(),ResourceArc::new(container)))
}
