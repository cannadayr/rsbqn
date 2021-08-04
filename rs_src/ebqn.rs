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

fn ge(arc: &ResourceArc<Container>,mut env: Id,mut i: Id) -> Id {
    loop {
        match i {
            0 => {
                break env
            },
            _ => {
                panic!("ge not matching");
                i -= 1; // unreachable
            }
        }
    }
}
fn set(arc: &ResourceArc<Container>,d: Id,i: Entity,v: Entity) -> Entity {
    match i {
        Entity::Slot(eid,sid) => {
            // acquire mutex
            let mut state = arc.mutex.lock().unwrap();
            state.set(d,eid,sid,v);
            // drop mutex
            drop(state);
        }
        _ => panic!("didnt get a slot"),
    };
    return v;
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
            // no slot sanity checking for now
            11|12 => {
                let i = stack.pop();
                let ie = match i {
                    Some(entity) => entity,
                    None => panic!("got something wrong from stack"),
                };
                let v = stack.pop();
                let ve = match v {
                    Some(entity) => entity,
                    None => panic!("got something wrong from stack"),
                };
                set(arc,1,ie,ve);
                stack.push(ve);
            },
            14 => {
                let _ = stack.pop();
            },
            21 => {
                let x = prog.b[ptr];ptr+=1;
                let w = prog.b[ptr];ptr+=1;
                let t = ge(arc,env,x);
                let mut state = arc.mutex.lock().unwrap();
                stack.push(state.get(t,w));
            },
            22 => {
                let x = prog.b[ptr];ptr+=1;
                let w = prog.b[ptr];ptr+=1;
                let t = ge(arc,env,x);
                let r = Slot(t,w).to_entity();
                stack.push(r)
            },
            25 => {
                break match stack.len() {
                    1 => {
                        let r: Entity = stack[0];
                        r
                    },
                    _ => {
                        println!("{:?}",stack);
                        panic!("stack overflow")
                    }
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
