use crate::schema::{Container,Id,State,ok};
use std::sync::Mutex;
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Container>)> {
    let state = State::new();
    let mutex = Mutex::new(state);
    let container = Container { mutex };
    Ok((ok(),ResourceArc::new(container)))
}

#[rustler::nif]
fn st(arc: ResourceArc<Container>) -> NifResult<(Atom,Id)> {
    let state = arc.mutex.lock().unwrap();
    let id = state.id();
    Ok((ok(),id))
}

#[rustler::nif]
fn incr_st(arc: ResourceArc<Container>) -> NifResult<Atom> {
    let mut state = arc.mutex.lock().unwrap();
    state.incr();
    Ok(ok())
}

#[rustler::nif]
fn ls(b: Vec<Id>,o: Vec<Id>, s: Vec<Vec<Id>>) -> NifResult<(Atom,Vec<Id>,Vec<Id>,Vec<Vec<Id>>)> {
    Ok((ok(),b,o,s))
}
