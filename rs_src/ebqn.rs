use crate::schema::{Env,V,Vu,Vs,Vn,Block,BlockInst,Code,Calleable,set,new_scalar,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};

fn ge(env: Env,i: usize) -> Env {
    match i {
        0 => env,
        _ => panic!("ge not implemented for i > 0")
    }
}

fn call(a: Vn,x: Vn, w: Vn) -> Vs {
    match a {
        Some(v) => v.call(x,w),
        _ => panic!("unimplemented call"),
    }
}

fn derv(env: Env,code: &Cc<Code>,block: &Cc<Block>) -> Vs {
    match (block.typ,block.imm) {
        (0,true) => panic!("imm block"),
        (typ,_) => {
            let block_inst = BlockInst::new(env.clone(),code.clone(),typ,(*block).clone(),None);
            let r = Vs::Ref(Cc::new(Vu::BlockInst(block_inst)));
            r
        },
    }
}

pub fn vm(env: &Env,code: &Cc<Code>,block: &Cc<Block>,mut pos: usize,mut stack: Vec<Vs>) -> Vs {
    debug!("block (typ,imm,locals,pos) : ({},{},{},{})",block.typ,block.imm,block.locals,block.pos);
    loop {
        let op = code.bc[pos];pos+=1;
        debug!("dbging (op,pos) : {},{}",op,pos);
        match op {
            0 => {
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                stack.push(Vs::Ref(r))
            },
            // combine 11 & 12 for now
            11|12 => {
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(true,i,v); // rtns a reference to v
                stack.push(Vs::Ref(r));
            },
            14 => {
                let _ = stack.pop();
            },
            15 => {
                let x = code.bc[pos];pos+=1;
                let r = derv(env.clone(),&code,&code.blocks[x]);
                stack.push(r);
            },
            16 => {
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(Some(f.to_ref().clone()),Some(x.to_ref().clone()),None);
                stack.push(r);
            },
            17 => {
                let w = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                let r = call(Some(f.to_ref().clone()),Some(x.to_ref().clone()),Some(w.to_ref().clone()));
                stack.push(r);
            },
            21 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 21 (x,w):({},{})",x,w);
                let t = ge(env.clone(),x);
                stack.push(Vs::Ref(t.get(w)))
            },
            22 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 22 (x,w):({},{})",x,w);
                let t = ge(env.clone(),x);
                stack.push(Vs::Slot(t,w))
            },
            7 => {
                break match stack.len() {
                    1 => {
                        stack.pop().unwrap()
                    },
                    _ => {
                        panic!("stack overflow")
                    }
                };
            },
            _ => {
                panic!("unreachable op: {}",op);
            }
        }
    }
}

// same type signature as Code::New
fn run(bc: Vec<usize>,objs: Vec<V>,blocks_raw: Vec<(u8,u8,usize)>,bodies_raw: Vec<(usize,usize)>) -> f64 {
    let code = Code::new(bc,objs,blocks_raw,bodies_raw);
    let root = Env::new(None,&code.blocks[0],None);
    let rtn = vm(&root,&code,&code.blocks[0],code.blocks[0].pos,Vec::new());
    match **rtn.to_ref() {
        Vu::Scalar(n) => n,
        _ => panic!("run failed"),
    }
}

// TODO convert test to macro
fn test(a: u64,b: f64) {
    assert_eq!(a as f64,b)
}

#[rustler::nif]
fn tests() -> NifResult<Atom> {
    test(5,run(vec![0,0,7],vec![new_scalar(5)],vec![(0,1,0)],vec![(0,0)]));
    Ok(ok())
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Env>,Vs)> {
    let code = Code::new(vec![0,0,7],vec![new_scalar(5)],vec![(0,1,0)],vec![(0,0)]); // 3
    let root = Env::new(None,&code.blocks[0],None);
    let rtn = vm(&root,&code,&code.blocks[0],code.blocks[0].pos,Vec::new());
    Ok((ok(),ResourceArc::new(root),rtn))
}
