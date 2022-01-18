pub mod schema;
mod late_init;
mod trace;
pub mod ebqn;
pub mod prim;
pub mod code;
mod fmt;
// use syslog & log-panics for erlang side logging, otherwise env_logger
use syslog::Facility;
extern crate log_panics;

// https://docs.rs/env_logger/0.7.1/env_logger/#capturing-logs-in-tests
pub fn init_log() {
    let _ = env_logger::builder().is_test(true).try_init();
}
