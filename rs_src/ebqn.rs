use crate::schema::{LateInit,V,Block,Code,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let mut state = State::new();

    let code0 = Code::default();
    let bc = vec![0,0,25];
    let code1 = Code {bc: bc, ..code0};
    let code1arc = Arc::new(code1);
    let block0 = Arc::new(Block::default());
    //[0,0,25],[5],[[0,1,0,0]]
    block0.code.init(&code1arc);
    let blocks = vec![block0];
    code1arc.blocks.init(&blocks);

    let mutex = Mutex::new(state);
    let container = Container { mutex };

    Ok((ok(),ResourceArc::new(container)))
}
