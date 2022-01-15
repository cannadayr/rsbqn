use log::{info};
use ebqn::init_log;
use ebqn::ebqn::{run,call,runtime,prog};
use ebqn::code::{c};
use ebqn::schema::{new_string,V};

fn main() {
    init_log();
    let runtime = runtime();
    let compiler = c(&runtime);
    let src = new_string("{×´1+↕𝕩}");
    let prog = prog(compiler,src,runtime);
    info!("func loaded");
    let result = call(1,Some(&run(prog)),Some(&V::Scalar(10.0)),None);
    info!("result = {}",&result);
}
