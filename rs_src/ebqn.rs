use crate::schema::{Env,V,Vu,Vs,Vn,Block,BlockInst,Code,Calleable,Body,set,new_scalar,new_body,ok};
use rustler::{Atom,NifResult};
use rustler::resource::ResourceArc;
use cc_mt::{Cc, Trace, Tracer, collect_cycles};
use log::{debug, trace, error, log_enabled, info, Level};
use std::panic;

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
    debug!("block (typ,imm,body) : ({},{},{:?})",block.typ,block.imm,block.body);
    loop {
        let op = code.bc[pos];pos+=1;
        debug!("dbging (op,pos) : {},{}",op,pos);
        match op {
            0 => {
                let x = code.bc[pos];pos+=1;
                let r = code.objs[x].clone();
                stack.push(Vs::Ref(r))
            },
            // combine 48 & 49 for now
            48|49 => {
                let i = stack.pop().unwrap();
                let v = stack.pop().unwrap();
                let r = set(true,i,v); // rtns a reference to v
                stack.push(Vs::Ref(r));
            },
            6 => {
                let _ = stack.pop();
            },
            1 => {
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
            34 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 34 (x,w):({},{})",x,w);
                let t = ge(env.clone(),x);
                stack.push(Vs::Ref(t.get(w)))
            },
            33 => {
                let x = code.bc[pos];pos+=1;
                let w = code.bc[pos];pos+=1;
                debug!("opcode 33 (x,w):({},{})",x,w);
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

fn run(code: Cc<Code>) -> f64 {
    let root = Env::new(None,&code.blocks[0],None);
    let (pos,_locals) =
        match *code.blocks[0].body {
            Body::Imm(b) => code.bodies[b],
            Body::Defer(_,_) => panic!("cant run deferred block"),
        };
    let rtn = vm(&root,&code,&code.blocks[0],pos,Vec::new());
    match **rtn.to_ref() {
        Vu::Scalar(n) => n,
        _ => panic!("run failed"),
    }
}

#[rustler::nif]
fn tests() -> NifResult<Atom> {
panic::catch_unwind(|| { assert_eq!(5.0,run(Code::new(vec![0,0,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,0)]))); }); //   0 PUSH, 25 RETN
panic::catch_unwind(|| { assert_eq!(3.0,run(Code::new(vec![0,0,6,0,1,7],vec![new_scalar(4.0),new_scalar(3.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,0)]))); }); //  14 POPS
panic::catch_unwind(|| { assert_eq!(5.0,run(Code::new(vec![0,0,33,0,0,48,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,1)]))); }); //  22 LOCM, 11 SETN
panic::catch_unwind(|| { assert_eq!(4.0,run(Code::new(vec![0,0,33,0,0,48,6,0,1,33,0,0,49,7],vec![new_scalar(5.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,1)]))); }); //  12 SETU
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,0,33,0,0,48,6,0,1,33,0,1,48,6,34,0,0,7],vec![new_scalar(2.0),new_scalar(3.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,2)]))); }); //  21 LOCO
panic::catch_unwind(|| { assert_eq!(1.0,run(Code::new(vec![0,0,33,0,0,48,6,0,1,34,0,0,16,7],vec![new_scalar(1.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,1)]))); }); //  16 FN1O
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,0,33,0,0,48,6,0,2,34,0,0,0,1,17,7],vec![new_scalar(2.0),new_scalar(3.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,1)]))); }); //  17 FN2O
panic::catch_unwind(|| { assert_eq!(6.0,run(Code::new(vec![0,0,1,1,16,7,34,0,1,7],vec![new_scalar(6.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1)))],vec![(0,0),(6,3)]))); }); //  15 DFND
panic::catch_unwind(|| { assert_eq!(3.0,run(Code::new(vec![1,1,33,0,0,48,6,0,1,34,0,0,0,0,17,7,34,0,2,7],vec![new_scalar(3.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Defer(vec![],vec![1])))],vec![(0,1),(16,3)]))); }); //     dyadic block function
panic::catch_unwind(|| { assert_eq!(7.0,run(Code::new(vec![0,0,0,1,11,2,33,0,0,33,0,1,12,2,48,6,34,0,0,7],vec![new_scalar(7.0),new_scalar(2.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,2)]))); }); //   3 ARRO,  4 ARRM
panic::catch_unwind(|| { assert_eq!(4.0,run(Code::new(vec![1,1,0,0,26,7,34,0,1,7],vec![new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0))),(1,true,new_body(Body::Imm(1)))],vec![(0,0),(6,2)]))); }); //   7 OP1D
panic::catch_unwind(|| { assert_eq!(6.0,run(Code::new(vec![0,1,1,1,0,0,26,16,7,34,0,4,6,34,0,1,7],vec![new_scalar(4.0),new_scalar(6.0)],vec![(0,true,new_body(Body::Imm(0))),(1,false,new_body(Body::Imm(1)))],vec![(0,0),(9,5)]))); }); //     deferred modifier
panic::catch_unwind(|| { assert_eq!(1.0,run(Code::new(vec![0,1,1,1,1,2,0,0,27,16,7,34,0,1,7,34,0,2,7],vec![new_scalar(3.0),new_scalar(1.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(2,true,new_body(Body::Imm(2)))],vec![(0,0),(11,3),(15,3)]))); }); //   8 OP2D
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,1,1,1,1,2,0,0,26,20,16,7,34,0,1,7,34,0,1,7],vec![new_scalar(2.0),new_scalar(3.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(1,true,new_body(Body::Imm(2)))],vec![(0,0),(12,3),(16,2)]))); }); //   9 TR2D
panic::catch_unwind(|| { assert_eq!(3.0,run(Code::new(vec![0,1,1,1,1,2,20,0,0,17,7,34,0,2,34,0,1,11,2,7,34,0,1,33,0,3,33,0,4,12,2,48,6,34,0,3,7],vec![new_scalar(3.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Defer(vec![],vec![1]))),(0,false,new_body(Body::Imm(2)))],vec![(0,0),(11,3),(20,5)]))); }); //     dyadic 2-train
panic::catch_unwind(|| { assert_eq!(4.0,run(Code::new(vec![0,1,1,1,1,2,1,3,21,0,0,17,7,34,0,2,7,34,0,1,7,34,0,2,34,0,1,11,2,7],vec![new_scalar(4.0),new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Defer(vec![],vec![1]))),(0,false,new_body(Body::Imm(2))),(0,false,new_body(Body::Defer(vec![],vec![3])))],vec![(0,0),(13,3),(17,3),(21,3)]))); }); //  19 TR3O
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,1,1,1,1,2,0,0,21,16,33,0,0,33,0,1,12,2,48,6,34,0,0,7,34,0,1,7,34,0,2,34,0,1,11,2,7],vec![new_scalar(2.0),new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(0,false,new_body(Body::Defer(vec![],vec![2])))],vec![(0,2),(24,3),(28,3)]))); }); //     monadic, data in left branch
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,2,33,0,0,48,1,1,1,2,1,3,21,16,7,0,1,33,1,0,49,6,34,0,1,7,34,0,1,6,32,1,0,7,0,0,33,1,0,49,6,34,0,1,7],vec![new_scalar(2.0),new_scalar(3.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(0,false,new_body(Body::Imm(2))),(0,false,new_body(Body::Imm(3)))],vec![(0,1),(15,3),(26,3),(34,3)]))); }); //  ordering
panic::catch_unwind(|| { assert_eq!(8.0,run(Code::new(vec![0,0,33,0,0,48,6,0,1,1,1,33,0,0,50,6,34,0,0,7,34,0,1,7],vec![new_scalar(3.0),new_scalar(8.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1)))],vec![(0,1),(20,3)]))); }); //  13 SETM
panic::catch_unwind(|| { assert_eq!(5.0,run(Code::new(vec![0,0,33,0,0,48,6,0,2,1,1,33,0,0,50,7,34,0,2,6,0,1,7],vec![new_scalar(4.0),new_scalar(5.0),new_scalar(6.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1)))],vec![(0,1),(16,3)]))); }); //     returns new value
panic::catch_unwind(|| { assert_eq!(4.0,run(Code::new(vec![0,0,0,1,11,2,33,0,0,33,0,1,12,2,48,6,0,2,1,1,33,0,0,33,0,1,12,2,50,6,34,0,0,7,34,0,1,34,0,2,11,2,7],vec![new_scalar(2.0),new_scalar(1.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Defer(vec![],vec![1])))],vec![(0,2),(34,3)]))); }); //     lists
panic::catch_unwind(|| { assert_eq!(1.0,run(Code::new(vec![0,0,33,0,0,48,6,1,1,6,34,0,0,7,0,1,33,0,0,48,7],vec![new_scalar(1.0),new_scalar(2.0)],vec![(0,true,new_body(Body::Imm(0))),(0,true,new_body(Body::Imm(1)))],vec![(0,1),(14,1)]))); });
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,0,33,0,0,48,6,1,1,6,32,0,0,7,0,1,33,1,0,49,7],vec![new_scalar(1.0),new_scalar(2.0)],vec![(0,true,new_body(Body::Imm(0))),(0,true,new_body(Body::Imm(1)))],vec![(0,1),(14,0)]))); });
panic::catch_unwind(|| { assert_eq!(6.0,run(Code::new(vec![1,1,33,0,0,33,0,1,12,2,48,6,0,1,34,0,0,16,6,0,2,34,0,1,16,7,0,0,33,0,0,48,6,1,2,1,3,11,2,7,34,0,1,33,1,0,49,7,34,0,1,6,32,1,0,7],vec![new_scalar(2.0),new_scalar(6.0),new_scalar(0.0)],vec![(0,true,new_body(Body::Imm(0))),(0,true,new_body(Body::Imm(1))),(0,false,new_body(Body::Imm(2))),(0,false,new_body(Body::Imm(3)))],vec![(0,2),(26,1),(40,3),(48,3)]))); });
panic::catch_unwind(|| { assert_eq!(5.0,run(Code::new(vec![1,1,33,0,0,48,6,0,0,32,0,0,16,32,0,0,16,34,0,0,16,1,2,16,7,1,3,34,0,1,26,7,34,0,0,34,0,1,16,7,34,0,4,34,0,1,16,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(0,false,new_body(Body::Imm(2))),(1,false,new_body(Body::Imm(3)))],vec![(0,1),(25,3),(32,3),(40,5)]))); });
panic::catch_unwind(|| { assert_eq!(3.0,run(Code::new(vec![1,1,33,0,0,48,6,0,1,32,0,0,0,0,26,16,34,0,0,1,2,26,16,1,3,16,7,34,0,4,1,4,34,0,1,26,20,7,34,0,1,7,34,0,0,34,0,1,16,7,34,0,4,34,0,1,16,7],vec![new_scalar(3.0),new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0))),(1,false,new_body(Body::Imm(1))),(0,false,new_body(Body::Imm(2))),(0,false,new_body(Body::Imm(3))),(1,false,new_body(Body::Imm(4)))],vec![(0,1),(27,5),(38,3),(42,3),(50,5)]))); });
panic::catch_unwind(|| { assert_eq!(1.0,run(Code::new(vec![0,1,1,1,1,2,1,3,27,0,0,17,7,34,0,1,7,32,0,1,34,0,2,1,4,34,0,1,26,21,7,34,0,2,7,34,0,2,34,0,4,34,0,1,17,7],vec![new_scalar(1.0),new_scalar(0.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(2,true,new_body(Body::Imm(2))),(0,false,new_body(Body::Defer(vec![],vec![3]))),(1,false,new_body(Body::Defer(vec![],vec![4])))],vec![(0,0),(13,3),(17,3),(31,3),(35,5)]))); }); //  0≠1 via Church booleans
panic::catch_unwind(|| { assert_eq!(2.0,run(Code::new(vec![0,0,0,1,0,2,0,3,0,4,1,1,11,2,11,2,11,2,11,2,11,2,1,2,0,0,0,0,1,3,11,2,11,2,26,16,7,34,0,1,7,34,0,1,1,4,16,7,34,0,1,7,34,0,1,33,0,3,33,0,4,12,2,48,6,34,0,0,33,0,5,48,6,0,0,1,5,1,6,26,16,6,1,7,32,0,4,32,0,5,16,26,7,34,0,1,32,1,4,16,7,34,0,0,6,1,8,33,1,5,49,7,34,0,1,33,0,5,33,0,6,12,2,48,6,34,0,6,34,0,4,16,7,34,0,0,6,1,9,7,34,0,1,33,0,3,33,0,4,12,2,48,6,34,0,3,7],vec![new_scalar(0.0),new_scalar(1.0),new_scalar(2.0),new_scalar(3.0),new_scalar(4.0)],vec![(0,true,new_body(Body::Imm(0))),(0,false,new_body(Body::Imm(1))),(1,true,new_body(Body::Imm(2))),(0,false,new_body(Body::Imm(3))),(0,false,new_body(Body::Imm(4))),(1,true,new_body(Body::Imm(5))),(0,false,new_body(Body::Imm(6))),(1,false,new_body(Body::Imm(7))),(0,false,new_body(Body::Imm(8))),(0,false,new_body(Body::Imm(9)))],vec![(0,0),(37,3),(41,2),(48,3),(52,6),(93,2),(101,3),(112,7),(133,3),(140,5)]))); });
    Ok(ok())
}

#[rustler::nif]
fn init_st() -> NifResult<(Atom,ResourceArc<Env>,Vs)> {
    let code = Code::new(vec![0,0,7],vec![new_scalar(5.0)],vec![(0,true,new_body(Body::Imm(0)))],vec![(0,0)]);
    let root = Env::new(None,&code.blocks[0],None);
    panic!("cant init anything");
    //let rtn = vm(&root,&code,&code.blocks[0],code.blocks[0].pos,Vec::new());
    //Ok((ok(),ResourceArc::new(root),rtn))
}
