use rustler::{Atom,Env,NifStruct,NifResult,Term};
use rustler::resource::ResourceArc;
rustler::atoms!{ok}

type Id = u64;

#[derive(NifStruct)]
#[module = "ebqn"]
pub struct State {
    root: Id
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<State>)> {
    let id : Id = 0;
    let state = State {root: id};
    Ok((ok(),ResourceArc::new(state)))
}

fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(State, env);
    true
}
rustler::init!("ebqn", [init_st],load=load);
