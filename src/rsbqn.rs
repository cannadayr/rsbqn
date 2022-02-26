use log::{info};
use core::f64::{INFINITY,NEG_INFINITY};
use rsbqn::init_log;
use rsbqn::vm::{run,call,runtime,prog,formatter};
use rsbqn::gen::code::{r0,r1,c,f};
use rsbqn::schema::{new_string,new_char,new_scalar,Body,Code,Env,V,Vs,Vn,Stack,A};
use rsbqn::provide::{provide,decompose,prim_ind};
use rsbqn::fmt::{fmt_result};
use rustyline::{Editor, Result};
use rustyline::error::ReadlineError;
use bacon_rajan_cc::Cc;

#[cfg(feature = "dhat")]
use dhat::{Dhat, DhatAlloc};

#[cfg(feature = "dhat")]
#[global_allocator]
static ALLOCATOR: DhatAlloc = DhatAlloc;

fn main() -> Result<()> {
    init_log();
    #[cfg(feature = "dhat")]
    let _dhat = Dhat::start_heap_profiling();

    let mut stack = Stack::new();
    let root = Env::new_root();

    let runtime = runtime(Some(&root),&mut stack).expect("couldnt load runtime");
    let compiler = run(Some(&root),&mut stack,c(&runtime)).expect("couldnt load compiler");
    let fmt = formatter(Some(&root),&mut stack,&runtime).expect("couldnt load formatter");
    // initialize vars/names/redef to empty arrays of size 0
    let vars = V::A(Cc::new(A::new(vec![],vec![0])));
    let names = V::A(Cc::new(A::new(vec![],vec![0])));
    let redef = V::A(Cc::new(A::new(vec![],vec![0])));

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let src = new_string(&line);
                match prog(&mut stack,&compiler,src,&runtime,&vars,&names,&redef) {
                    Ok(prog) => {
                        match run(Some(&root),&mut stack,prog) {
                            Ok(exec) => {
                                match call(&mut stack,0,Vn(Some(&exec)),Vn(None),Vn(None)) {
                                    Ok(r) => {
                                        match call(&mut stack,1,Vn(Some(&fmt)),Vn(Some(&r.into_v().unwrap())),Vn(None)) {
                                            Ok(f) => println!("{}",fmt_result(&f.into_v().unwrap().into_a().unwrap())),
                                            Err(e) => println!("{}",e),
                                        }
                                    },
                                    Err(e) => println!("{}",e),
                                };
                            },
                            Err(e) => println!("{}",e),
                        };
                    },
                    Err(e) => println!("{}",e),
                };
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

    Ok(())
    // single line variations for copy-pasting
    //{ let result = call(1,Some(&run(prog(&compiler,new_string("{×´1+↕𝕩}"),&runtime))),Some(&V::Scalar(10.0)),None); println!("{}",result); }
    //{ let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();let compiler = c(&runtimev); }
    //{ let runtimev = runtime(); }
    //{ let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();let compiler = c(&runtimev); call(1,Some(&run(prog(&compiler,src,&runtime))),Some(&V::Scalar(10.0)),None); }
    //{ let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();let compiler = c(&runtimev); let result = call(1,Some(&run(prog(&compiler,src,&runtime))),Some(&V::Scalar(10.0)),None); println!("{}",result); }

}
