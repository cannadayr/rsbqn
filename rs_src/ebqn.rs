use crate::schema::{A,B,LateInit,V,Block,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let mut state = State::new();
    let mutex = Mutex::new(state);
    let container = Container { mutex };
    let a = A::default();
    let b = B::default();
    a.b.init(&b);
    b.a.init(&a);
    //println!("{:?}", a.b.a.b.a);
    Ok((ok(),ResourceArc::new(container)))
}
