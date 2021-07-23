use crate::schema::{Block,Container,Entity,Env,Id,Prog,Slot,State,ToEntity,ok};
use std::sync::Mutex;
use std::mem::drop;
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

fn vm(arc: &ResourceArc<Container>,prog: Prog,bl: Block,env: Id,mut ptr: usize,mut stack: Vec<Entity>) -> Entity {
    loop {
        let op = prog.b[ptr];ptr+=1;
        match op {
            0 => {
                let x = prog.b[ptr];ptr+=1;
                let r: Entity = prog.o[x].to_entity();
                stack.push(r)
            },
            14 => {
                let _ = stack.pop();
            },
            25 => {
                break match stack.len() {
                    1 => {
                        let r: Entity = stack[0];
                        r
                    },
                    _ => panic!("stack overflow")
                };
            }
            _ => panic!("illegal instruction"),
        };
    }
}

#[rustler::nif]
fn run(arc: ResourceArc<Container>,b: Vec<usize>,o: Vec<Id>, s: Vec<Vec<Id>>) -> NifResult<(Atom,Entity)> {
    let blocks: Vec<Block> = s.iter().map(|bl| Block::new(bl.to_vec())).collect();
    let bl: Block = blocks[0];
    let prog = Prog { b: b, o: o, s: blocks };
    let mut state = arc.mutex.lock().unwrap();
    let parent = state.root();
    let e: Env = Env::new(parent,bl.l);
    let id: Id = state.alloc(e);
    drop(state);
    let stack: Vec<Entity> = Vec::new();
    //let rtn =
    //    match vm(&arc,prog,bl,id,bl.st,stack) {
    //        Entity::Id(id) => id
    //    };
    let rtn = vm(&arc,prog,bl,id,bl.st,stack);
    Ok((ok(),rtn))
}
