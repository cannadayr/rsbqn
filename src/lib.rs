pub mod schema;
mod late_init;
mod trace;
pub mod vm;
pub mod provide;
pub mod gen { pub mod code; }
mod fmt;

// https://docs.rs/env_logger/0.7.1/env_logger/#capturing-logs-in-tests
pub fn init_log() {
    let _ = env_logger::builder().is_test(true).try_init();
}
