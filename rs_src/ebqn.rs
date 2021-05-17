use crate::schema::{Block,Container,Id,State,ok};
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

fn load_vm(arc: &ResourceArc<Container>,b: Vec<Id>,o: Vec<Id>, s: Vec<Block>,bl: Block,id: Id) -> Id {
    1111
}
#[rustler::nif]
fn run(arc: ResourceArc<Container>,b: Vec<Id>,o: Vec<Id>, s: Vec<Vec<Id>>) -> NifResult<(Atom,Id)> {
    let blocks: Vec<Block> = s.iter().map(|bl| Block::new(bl.to_vec())).collect();
    let block: Block = blocks[0];
    let state = arc.mutex.lock().unwrap();
    let root_id = state.root();
    let rtn = load_vm(&arc,b,o,blocks,block,root_id);
    Ok((ok(),rtn))
}
