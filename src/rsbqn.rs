use log::{info};
use core::f64::{INFINITY,NEG_INFINITY};
use rsbqn::init_log;
use rsbqn::vm::{run,call,runtime,prog};
use rsbqn::gen::code::{r0,r1,c};
use rsbqn::schema::{new_string,new_char,new_scalar,Body,Code,Env,V,Vs,Vn,Stack};
use rsbqn::provide::{provide,decompose,prim_ind};

#[cfg(feature = "dhat")]
use dhat::{Dhat, DhatAlloc};

#[cfg(feature = "dhat")]
#[global_allocator]
static ALLOCATOR: DhatAlloc = DhatAlloc;

fn main() {
    init_log();
    #[cfg(feature = "dhat")]
    let _dhat = Dhat::start_heap_profiling();

    let mut stack = Stack::new();
    let root = Env::new_root();

    // each function one-at-a-time
    let runtime = runtime(Some(&root),&mut stack);
    let compiler = run(Some(&root),&mut stack,c(&runtime));
    let src = new_string("{×´1+↕𝕩}");
    let prog = match prog(&mut stack,&compiler,src,&runtime) {
        Ok(p) => p,
        Err(e) => panic!("couldn't load prog"),
    };
    let exec = run(Some(&root),&mut stack,prog);
    let result = match call(&mut stack,1,Vn(Some(&exec)),Vn(Some(&V::Scalar(10.0))),Vn(None)) {
        Ok(r) => println!("{}",r),
        Err(e) => panic!("something went wrong"),
    };

    // single line variations for copy-pasting
    //{ let result = call(1,Some(&run(prog(&compiler,new_string("{×´1+↕𝕩}"),&runtime))),Some(&V::Scalar(10.0)),None); println!("{}",result); }
    //{ let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();let compiler = c(&runtimev); }
    //{ let runtimev = runtime(); }
    //{ let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();let compiler = c(&runtimev); call(1,Some(&run(prog(&compiler,src,&runtime))),Some(&V::Scalar(10.0)),None); }
    //{ let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();let compiler = c(&runtimev); let result = call(1,Some(&run(prog(&compiler,src,&runtime))),Some(&V::Scalar(10.0)),None); println!("{}",result); }

}
