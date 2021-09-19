mod schema;
mod ebqn;
use rustler::{Env,Term};

pub fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(schema::Env, env);
    env_logger::init();
    true
}
rustler::init!("ebqn", [ebqn::init_st,ebqn::tests],load=load);
