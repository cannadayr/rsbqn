// this will need to be cleaned up before it compiles
use rustler::{Encoder};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use rustler::{Env,Term};
rustler::atoms!{ok}

impl Encoder for V {
    fn encode<'a>(&self, env: rustler::Env<'a>) -> rustler::Term<'a> {
        match self {
            V::Scalar(n) => n.encode(env),
            V::Char(_c) => panic!("can't encode char to BEAM"),
            V::BlockInst(_b,_prim) => panic!("can't encode blockinst to BEAM"),
            V::UserMd1(_b,_a,_prim) => panic!("can't encode UserMd1 to BEAM"),
            V::UserMd2(_b,_a,_prim) => panic!("can't encode UserMd2 to BEAM"),
            V::Nothing => panic!("can't encode nothing to BEAM"),
            V::A(_a) => panic!("can't encode array to BEAM"),
            V::Fn(_a,_prim) => panic!("can't encode fn to BEAM"),
            V::R1(_f,_prim) => panic!("can't encode r1 to BEAM"),
            V::R2(_f,_prim) => panic!("can't encode r2 to BEAM"),
            V::D1(_d1,_prim) => panic!("can't encode d1 to BEAM"),
            V::D2(_d2,_prim) => panic!("can't encode d2 to BEAM"),
            V::Tr2(_tr2,_prim) => panic!("can't encode train2 to BEAM"),
            V::Tr3(_tr3,_prim) => panic!("can't encode train3 to BEAM"),
        }
    }
}

#[rustler::nif]
fn init_r() -> NifResult<(Atom,ResourceArc<Runtime>)> {
    Ok((ok(),ResourceArc::new(Runtime(runtime()))))
}
#[rustler::nif]
fn init_c(r: ResourceArc<Runtime>) -> NifResult<(Atom,ResourceArc<Compiler>)> {
    let compiler = c(&r.0);
    Ok((ok(),ResourceArc::new(Compiler(compiler.as_block_inst().unwrap().0.clone()))))
}
#[rustler::nif]
fn compile(r: ResourceArc<Runtime>,c: ResourceArc<Compiler>,s: &str) -> NifResult<(Atom,ResourceArc<Prog>)> {
    info!("got src {:?}",&s);
    let src = new_string(s);
    let prog = prog(V::BlockInst(c.0.clone(),None),src,r.0.clone());
    Ok((ok(),ResourceArc::new(Prog(prog.clone()))))
}
#[rustler::nif]
fn callp(p: ResourceArc<Prog>,n: f64) -> NifResult<(Atom,V)> {
    let result = call(1,Some(&run(p.0.clone())),Some(&V::Scalar(n)),None);
    Ok((ok(),result.into_v().unwrap()))
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
rustler::init!("ebqn", [ebqn::init_r,ebqn::init_c,ebqn::compile,ebqn::callp],load=load);
