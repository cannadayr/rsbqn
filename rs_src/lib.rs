mod schema;
mod late_init;
mod trace;
mod ebqn;
mod prim;
mod test;
mod code;
mod fmt;
use rustler::{Env,Term};
use syslog::Facility;
extern crate log_panics;

pub fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(schema::Env, env);
    let _r = syslog::init(Facility::LOG_USER,
                 log::LevelFilter::Info,
                 Some("ebqn"));
    log_panics::init();
    true
}
rustler::init!("ebqn", [ebqn::init_st],load=load);
