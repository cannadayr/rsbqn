mod schema;
mod ebqn;
mod test;
use rustler::{Env,Term};
use syslog::{Facility, Error};
extern crate log_panics;

pub fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(schema::Env, env);
    syslog::init(Facility::LOG_USER,
                 log::LevelFilter::Debug,
                 Some("ebqn"));
    log_panics::init();
    true
}
rustler::init!("ebqn", [ebqn::init_st,ebqn::tests],load=load);
