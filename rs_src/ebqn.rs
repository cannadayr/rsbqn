use crate::schema::{V,Block,Container,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use std::sync::Mutex;
use std::sync::Arc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let mut state = State::new();
    let mutex = Mutex::new(state);
    let container = Container { mutex };
    Ok((ok(),ResourceArc::new(container)))
}
