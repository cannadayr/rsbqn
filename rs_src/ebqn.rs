use crate::schema::{Id,State,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<State>)> {
    let id : Id = 0;
    let state = State {root: id};
    Ok((ok(),ResourceArc::new(state)))
}
