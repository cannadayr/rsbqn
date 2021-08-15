use crate::schema::{LateInit,V,Block,Code,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let mut state = State::new();

    let code = Arc::new(Code::default());
    let block = Arc::new(Block::default());

    block.code.init(&code);
    let blocks = vec![block];
    code.blocks.init(&blocks);

    let mutex = Mutex::new(state);
    let container = Container { mutex };

    Ok((ok(),ResourceArc::new(container)))
}
