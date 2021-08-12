use crate::schema::{Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let state = State {};
    let mutex = Mutex::new(state);
    let container = Container { mutex };
    Ok((ok(),ResourceArc::new(container)))
}
