use crate::schema::{LateInit,V,Block,Code,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let mut state = State::new();

    let code = Code::default();
    let block0 = Block::default();

    block0.code.init(&code);
    let blocks: Arc<[Block]> = vec![block0].into();
    let b = vec![0,0,25];
    let o = vec![5];
    let s = vec![vec![0,1,0,0]];
    //let blocks = Block
    //let a = A::default();
    //let b = B::default();
    //a.b.init(&b);
    //b.a.init(&a);
    //println!("{:?}", a.b.a.b.a);

    let mutex = Mutex::new(state);
    let container = Container { mutex };

    Ok((ok(),ResourceArc::new(container)))
}
