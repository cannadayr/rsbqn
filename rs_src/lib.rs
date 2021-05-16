mod schema;
mod ebqn;
use rustler::{Env,Term};

pub fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(schema::Container, env);
    true
}
rustler::init!("ebqn", [
    ebqn::init_st,
    ebqn::st,
    ebqn::incr_st,
    ebqn::ls,
],load=load);
