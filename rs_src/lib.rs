pub mod schema;
mod late_init;
mod trace;
pub mod ebqn;
mod prim;
pub mod code;
mod fmt;
use rustler::{Env,Term};
// use syslog & log-panics for erlang side logging, otherwise env_logger
use syslog::Facility;
extern crate log_panics;

// https://docs.rs/env_logger/0.7.1/env_logger/#capturing-logs-in-tests
pub fn init_log() {
    let _ = env_logger::builder().is_test(true).try_init();
}

pub fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(schema::Env, env);
    rustler::resource!(schema::Runtime, env);
    rustler::resource!(schema::Compiler, env);
    rustler::resource!(schema::Prog, env);
    let _r = syslog::init(Facility::LOG_USER,
                 log::LevelFilter::Info,
                 Some("ebqn"));
    log_panics::init();
    true
}
rustler::init!("ebqn", [/*ebqn::init_r,*/ebqn::init_c,/*ebqn::compile,*/ebqn::callp],load=load);
