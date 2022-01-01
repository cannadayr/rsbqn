use crate::schema::{A,V,Vn,Vs,Decoder,D1,D2,Tr2,Tr3};
use crate::ebqn::{call};
use cc_mt::Cc;
use std::cmp::max;
use log::{debug, trace, error, log_enabled, info, Level};
use std::iter::FromIterator;
use std::ops::Deref;
use std::char;
use itertools::Itertools;
use num_traits::{cast::FromPrimitive};

fn dbg_args(fun: &str, arity: usize, x: &Vn, w: &Vn) {
    match arity {
        1 => {
            info!("calling {}/{}: 𝕩 = {}",fun,arity,format!("{}",x.clone().unwrap().to_string()));
        },
        2 => {
            info!("calling {}/{}: 𝕩 = {};𝕨 = {}",fun,arity,format!("{}",x.clone().unwrap().to_string()),format!("{}",w.clone().unwrap().to_string()));
        },
        _ => ()
    };
}
fn dbg_rtn(fun: &str,arity: usize, r: &Vs) {
    info!("rtn     {}/{}: rtn = {}",fun,arity,r);
}

// Type
fn typ(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::Scalar(_n) => Vs::V(V::Scalar(1.0)),
            V::A(_a) => Vs::V(V::Scalar(0.0)),
            V::Char(_c) => Vs::V(V::Scalar(2.0)),
            V::DervBlockInst(_b,_a,_prim) => Vs::V(V::Scalar(3.0)),
            V::D1(_d1,_prim) => Vs::V(V::Scalar(3.0)),
            V::D2(_d2,_prim) => Vs::V(V::Scalar(3.0)),
            V::Tr2(_tr3,_prim) => Vs::V(V::Scalar(3.0)),
            V::Tr3(_tr3,_prim) => Vs::V(V::Scalar(3.0)),
            V::Fn(_fn,_prim) => Vs::V(V::Scalar(3.0)),
            V::R1(_r1,_prim) => Vs::V(V::Scalar(4.0)),
            V::R2(_r2,_prim) => Vs::V(V::Scalar(5.0)),
            V::BlockInst(b,_prim) => Vs::V(V::Scalar(b.def.typ as f64 + 3.0)),
            _ => panic!("no matching value for typ"),
        },
        _ => panic!("typ not implemented"),
    }
}
// Fill
fn fill(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => Vs::V(V::Scalar(0.0)),
        2 => Vs::V(x.unwrap()),
        _ => panic!("illegal fill arity"),
    }
}
// Log
fn log(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::Scalar(xs) => Vs::V(V::Scalar(xs.ln())),
            _ => panic!("monadic log expected number"),
        },
        2 => match (x.unwrap(),w.unwrap()) {
            (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(xs.ln() / ws.ln())),
            _ => panic!("dyadic log expected numbers"),
        },
        _ => panic!("illegal power arity"),
    }
}
// GroupLen
fn group_len(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => {
            match x.unwrap() {
                V::A(xa) => {
                    let l = xa.r.iter().fold(-1.0, |acc, i| i.to_f64().max(acc));
                    let s = l + 1.0;
                    let mut r = vec![V::Scalar(0.0);s.clone() as usize];
                    let mut i = 0;
                    while i < xa.r.len() {
                        let e = xa.r[i].to_f64();
                        if e >= 0.0 {
                            r[e as usize] = V::Scalar(r[e as usize].to_f64() + 1.0)
                        }
                        i += 1;
                    }
                    Vs::V(V::A(Cc::new(A::new(r.clone(),vec![r.len() as usize]))))
                },
                _ => panic!("group_len 𝕩 is not an array"),
            }
        },
        2 => {
            match (x.unwrap(),w.unwrap()) {
                (V::A(xa),V::Scalar(ws)) => {
                    let l = xa.r.iter().fold(ws-1.0, |acc, i| i.to_f64().max(acc));
                    let s = l + 1.0;
                    let mut r = vec![V::Scalar(0.0);s.clone() as usize];
                    let mut i = 0;
                    while i < xa.r.len() {
                        let e = xa.r[i].to_f64();
                        if e >= 0.0 {
                            r[e as usize] = V::Scalar(r[e as usize].to_f64() + 1.0)
                        }
                        i += 1;
                    }
                    Vs::V(V::A(Cc::new(A::new(r.clone(),vec![r.len() as usize]))))
                },
                _ => panic!("group_len 𝕩 is not an array"),
            }
        },
        _ => panic!("illegal group_len arity"),
    }
}
// GroupOrd
fn group_ord(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        2 => {
            match (&x.unwrap(),&w.unwrap()) {
                (V::A(xa),V::A(wa)) => {
                    let (mut s,l) = wa.r.iter().fold((vec![],0.0), |(mut si,li), v| { si.push(li); (si,li as f64 + v.to_f64()) });
                    let mut r = vec![V::Nothing;l as usize];
                    xa.r.iter().enumerate().for_each(|(i,e)| if e.to_f64() >= 0.0  {
                        r[s[e.to_f64() as usize] as usize] = V::Scalar(i as f64);
                        s[e.to_f64() as usize] += 1.0;
                    });
                    let shape = vec![r.len().clone()];
                    Vs::V(V::A(Cc::new(A::new(r.clone(),shape))))
                },
                _ => panic!("dyadic group_ord x is not an array"),
            }
        },
        _ => panic!("illegal group_ord arity"),
    }
}
// !
fn assert_fn(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("assert_fn",arity,&x,&w);
    let r =
    match arity {
        1 => match x.unwrap().as_scalar() {
            Some(n) if *n == 1.0 => Vs::V(V::Scalar(1.0)),
            _ => panic!("assert failed"),
        },
        2 => match x.unwrap().as_scalar() {
            Some(n) if *n == 1.0 => Vs::V(V::Scalar(1.0)),
            _ => {
                let msg = w.unwrap().to_array().r.iter().map(|e| match e {
                    V::Char(c) => *c,
                    _ => panic!("panic is not a string"),
                }).collect::<Vec<char>>();
                panic!("{}",String::from_iter(&msg));
            },
        },
        _ => panic!("illegal assert arity"),
    };
    //dbg_rtn("assert_fn",arity,&r);
    r
}
// +
pub fn plus(arity:usize, x: Vn,w: Vn) -> Vs {
    //dbg_args("plus",arity,&x,&w);
    let r =
    match arity {
        1 => Vs::V(x.unwrap()),
        2 => match (x.unwrap(),w.unwrap()) {
                (V::Char(xc),V::Scalar(ws)) if ws >= 0.0 => Vs::V(V::Char(char::from_u32(u32::from(xc) + u32::from_f64(ws).unwrap()).unwrap())),
                (V::Scalar(xs),V::Char(wc)) if xs >= 0.0 => Vs::V(V::Char(char::from_u32(u32::from(wc) + u32::from_f64(xs).unwrap()).unwrap())),
                (V::Char(xc),V::Scalar(ws)) if ws <  0.0 => Vs::V(V::Char(char::from_u32(u32::from(xc) - u32::from_f64(ws.abs()).unwrap()).unwrap())),
                (V::Scalar(xs),V::Char(wc)) if xs <  0.0 => Vs::V(V::Char(char::from_u32(u32::from(wc) - u32::from_f64(xs.abs()).unwrap()).unwrap())),
                (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(xs + ws)),
                _ => panic!("dyadic plus pattern not found"),
        },
        _ => panic!("illegal plus arity"),
    };
    //dbg_rtn("plus",arity,&r);
    r
}
// -
fn minus(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("minus",arity,&x,&w);
    let r =
    match arity {
        1 => match x.unwrap() {
            V::Scalar(xs) => Vs::V(V::Scalar(-1.0 * xs)),
            _ => panic!("monadic minus expected number"),
        },
        2 => match (x.unwrap(),w.unwrap()) {
            (V::Scalar(xs),V::Char(wc)) => Vs::V(V::Char(char::from_u32(u32::from(wc) - u32::from_f64(xs).unwrap()).unwrap())),
            (V::Char(xc),V::Char(wc)) if u32::from(xc) > u32::from(wc) => Vs::V(V::Scalar(-1.0*f64::from(u32::from(xc) - u32::from(wc)))),
            (V::Char(xc),V::Char(wc)) => Vs::V(V::Scalar(f64::from(u32::from(wc) - u32::from(xc)))),
            (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(ws - xs)),
            _ => panic!("dyadic minus pattern not found"),
        },
        _ => panic!("illegal minus arity"),
    };
    //dbg_rtn("minus",arity,&r);
    r
}
// ×
fn times(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("times",arity,&x,&w);
    match arity {
        2 => match (x.unwrap(),w.unwrap()) {
            (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(ws * xs)),
            _ => panic!("dyadic times illegal arguments"),
        },
        _ => panic!("illegal times arity"),
    }
}
// ÷
fn divide(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("divide",arity,&x,&w);
    match arity {
        1 => match x.unwrap() {
            V::Scalar(xs) => Vs::V(V::Scalar(1.0 / xs)),
            _ => panic!("monadic divide expected number"),
        },
        2 => match (x.unwrap(),w.unwrap()) {
            (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(ws / xs)),
            _ => panic!("dyadic divide expected number"),
        },
        _ => panic!("illegal divide arity"),
    }
}
// ⋆
fn power(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::Scalar(xs) => Vs::V(V::Scalar(xs.exp())),
            _ => panic!("monadic power expected number"),
        },
        2 => match (x.unwrap(),w.unwrap()) {
            (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(ws.powf(xs))),
            _ => panic!("dyadic power expected numbers"),
        },
        _ => panic!("illegal power arity"),
    }
}
// ⌊
fn floor(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1|2 => Vs::V(V::Scalar(x.unwrap().to_f64().floor())),
        _ => panic!("illegal divide arity"),
    }
}
// =
fn equals(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("equals",arity,&x,&w);
    let r =
    match arity {
        1 => match x.unwrap() {
            V::A(xa) => Vs::V(V::Scalar(xa.sh.len() as i64 as f64)),
            V::Char(_xc) => Vs::V(V::Scalar(0.0)),
            V::Scalar(_xs) => Vs::V(V::Scalar(0.0)),
            V::DervBlockInst(_b,_a,_prim) => Vs::V(V::Scalar(0.0)),
            V::D2(_d2,_prim) => Vs::V(V::Scalar(0.0)),
            _ => panic!("monadic equals 𝕩 is not a valid value"),
        },
        2 => match x.unwrap() == w.unwrap() {
            true => Vs::V(V::Scalar(1.0)),
            false => Vs::V(V::Scalar(0.0)),
        },
        _ => panic!("illegal equals arity"),
    };
    //dbg_rtn("equals",arity,&r);
    r
}
// ≤
fn lesseq(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("lesseq",arity,&x,&w);
    let r =
    match arity {
        2 => {
            let t = typ(1,x.clone(),None).to_ref().to_f64();
            let s = typ(1,w.clone(),None).to_ref().to_f64();
            if (&x).as_ref().unwrap().clone().is_fn() || (&w).as_ref().unwrap().clone().is_fn() {
                panic!("cannot compare operations")
            };
            match t != s {
                true  => Vs::V(V::Scalar((s <= t) as i64 as f64)),
                false => Vs::V(V::Scalar((w.unwrap().to_f64() <= x.unwrap().to_f64()) as i64 as f64)),
            }
        },
        _ => panic!("illegal lesseq arity"),
    };
    //dbg_rtn("lesseq",arity,&r);
    r
}
// ≢
fn shape(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::A(xa) => {
                let ravel = xa.sh.iter().map(|n| V::Scalar(*n as i64 as f64)).collect::<Vec<V>>();
                let shape = vec![ravel.len()];
                Vs::V(V::A(Cc::new(A::new(ravel,shape))))
            },
            _ => panic!("shape 𝕩 is not an array"),
        },
        _ => panic!("illegal shape arity"),
    }
}
// ⥊
fn reshape(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => {
            match x.unwrap() {
                V::A(xa) => Vs::V(V::A(Cc::new(A::new(xa.r.clone(),vec![xa.r.len()])))),
                _ => panic!("monadic reshape no arr"),
            }
        },
        2 => {
            match (x.unwrap(),w.unwrap()) {
                (V::A(ax),V::A(aw)) => {
                    let sh = aw.r.iter().map(|e| match e {
                        V::Scalar(n) => *n as usize,
                        _ => panic!("W ravel is not a num"),
                    }).collect::<Vec<usize>>();
                    Vs::V(V::A(Cc::new(A::new(ax.r.clone(),sh))))
                },
                _ => panic!("dydic reshape no match"),
            }
        },
        _ => panic!("illegal reshape arity"),
    }
}
// ⊑
fn pick(arity: usize, x: Vn, w: Vn) -> Vs {
    //dbg_args("pick",arity,&x,&w);
    let r =
    match arity {
        2 => {
            match (x.unwrap(),w.unwrap()) {
                (V::A(a),V::Scalar(i)) if i >= 0.0 => Vs::V(a.r[i as i64 as usize].clone()),
                (V::A(a),V::Scalar(i)) if i <  0.0 => Vs::V(a.r[((a.r.len() as f64) + i) as i64 as usize].clone()),
                _ => panic!("pick - can't index into non array"),
            }
        },
        _ => panic!("illegal pick arity"),
    };
    //dbg_rtn("pick",arity,&r);
    r
}
// ↕
fn windows(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::Scalar(n) => Vs::V(V::A(Cc::new(A::new((0..n as i64).map(|v| V::Scalar(v as f64)).collect::<Vec<V>>(),vec![n as usize])))),
            _ => panic!("x is not a number"),
        },
        _ => panic!("illegal windows arity"),
    }

}
// ⌜
fn table(arity: usize, f: Vn, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::A(xa) => {
                let ravel = (*xa).r.iter().map(|e| call(arity,f.clone(),Some(e.clone()),None).to_ref().clone()).collect::<Vec<V>>();
                let sh = (*xa).sh.clone();
                Vs::V(V::A(Cc::new(A::new(ravel,sh))))
            },
            _ => panic!("monadic table x is not an array"),
        },
        2 => {
            match (x.unwrap(),w.unwrap()) {
                (V::A(xa),V::A(wa)) => {
                    let ravel = (*wa).r.iter().flat_map(|d| {
                        (*xa).r.iter().map(|e| call(arity,f.clone(),Some(e.clone()),Some(d.clone())).to_ref().clone()).collect::<Vec<V>>()
                    }).collect::<Vec<V>>();
                    let sh = (*wa).sh.clone().into_iter().chain((*xa).sh.clone().into_iter()).collect();
                    Vs::V(V::A(Cc::new(A::new(ravel,sh))))
                },
                _ => panic!("dyadic table not an array"),
            }
        },
        _ => panic!("illegal table arity"),
    }
}
// `
fn scan(arity: usize, f: Vn, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => {
            match x.unwrap() {
                V::A(a) => {
                    let s = &a.sh;
                    if s.len()==0 {
                        panic!("scan monadic array rank not at least 1");
                    };
                    let l = a.r.len();
                    let mut r = vec![V::Nothing;l];
                    if l > 0 {
                        let mut c = 1;
                        let mut i = 1;
                        while i < s.len() {
                            c *= s[i];
                            i += 1;
                        }
                        i = 0;
                        while i < c {
                            r[i] = a.r[i].clone();
                            i += 1;
                        }
                        while i < l {
                            r[i] = call(2,f.clone(),Some(a.r[i].clone()),Some(r[i-c].clone())).to_ref().clone();
                            i += 1;
                        }
                    };
                    Vs::V(V::A(Cc::new(A::new(r,s.to_vec()))))
                },
                _ => panic!("monadic scan x is not an array"),
            }
        },
        2 => {
            let (wr,wa) = match w.unwrap() {
                V::A(wa) => (wa.sh.len(),wa),
                // TODO `wa` doesn't actually need to be a ref counted array
                V::Scalar(ws) => (0,Cc::new(A::new(vec![V::Scalar(ws)],vec![1]))),
                _ => panic!("dyadic scan w is invalid type"),
            };
            match x.unwrap() {
                V::A(xa) => {
                    let s = &xa.sh;
                    if s.len()==0 {
                        panic!("scan dyadic array rank not at least 1");
                    };
                    if 1+wr != s.len() {
                        panic!("scan dyadic array rank don't match");
                    }
                    // TODO add test 'shape of 𝕨 must be cell shape of 𝕩' here
                    let l = xa.r.len();
                    let mut r = vec![V::Nothing;l];
                    if l > 0 {
                        let mut c = 1;
                        let mut i = 1;
                        while i < s.len() {
                            c *= s[i];
                            i += 1;
                        }
                        i = 0;
                        while i < c {
                            r[i] = call(2,f.clone(),Some(xa.r[i].clone()),Some(wa.r[i].clone())).to_ref().clone();
                            i += 1;
                        }
                        while i < l {
                            r[i] = call(2,f.clone(),Some(xa.r[i].clone()),Some(r[i-c].clone())).to_ref().clone();
                            i += 1;
                        }
                    };
                    Vs::V(V::A(Cc::new(A::new(r,s.to_vec()))))
                },
                _ => panic!("dyadic scan x or w is not an array"),
            }
        },
        _ => panic!("illegal scan arity"),
    }
}
// _fillBy_
fn fill_by(arity: usize, f: Vn, _g: Vn, x: Vn, w: Vn) -> Vs {
    call(arity,f,x,w)
}
// ⊘
fn cases(arity: usize, f: Vn, g: Vn, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => call(arity,f,x,None),
        2 => call(arity,g,x,w),
        _ => panic!("illegal cases arity"),
    }
}
// ⎊
fn catches(_arity: usize, _f: Vn, _g: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("catches not implemented");
}

pub fn decompose(arity:usize, x: Vn,_w: Vn) -> Vs {
    //dbg_args("decompose",arity,&x,&w);
    let r =
    match arity {
        1 => {
            if // atoms
                match (&x).as_ref().unwrap() {
                    V::Scalar(_n) => true,
                    V::Char(_c) => true,
                    V::Nothing => true,
                    V::A(_a) => true,
                    _ => false
                }
            {
                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(-1.0),(&x).as_ref().unwrap().clone()],vec![2]))))
            }
            else if // primitives
                match (&x).as_ref().unwrap() {
                    V::BlockInst(_b,Some(_prim)) => true,
                    V::DervBlockInst(_b,_a,Some(_prim)) => true,
                    V::Fn(_a,Some(_prim)) => true,
                    V::R1(_f,Some(_prim)) => true,
                    V::R2(_f,Some(_prim)) => true,
                    V::D1(_d1,Some(_prim)) => true,
                    V::D2(_d2,Some(_prim)) => true,
                    V::Tr2(_tr2,Some(_prim)) => true,
                    V::Tr3(_tr3,Some(_prim)) => true,
                    _ => false,
                }
            {
                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(0.0),(&x).as_ref().unwrap().clone()],vec![2]))))
            }
            else if // repr
                match (&x).as_ref().unwrap() {
                    V::DervBlockInst(_b,_a,None) => true,
                    _ => false,
                }
            {
                match (&x).as_ref().unwrap() {
                    V::DervBlockInst(b,a,None) => {
                        let t = 3 + b.def.typ;
                        match t {
                            4 => {
                                let (f,g) = a.iter().collect_tuple().unwrap();
                                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(4.0),g.as_ref().unwrap().clone(),f.as_ref().unwrap().clone()],vec![3]))))
                            },
                            5 => {
                                let (f,g,h) = a.iter().collect_tuple().unwrap();
                                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(5.0),g.as_ref().unwrap().clone(),f.as_ref().unwrap().clone(),h.as_ref().unwrap().clone()],vec![4]))))
                            },
                            _ => panic!("DervBlockInst illegal decompose"),
                        }
                    },
                    _ => panic!("decompose other"),
                }
            }
            else { // everything else
                match (&x).as_ref().unwrap() {
                    V::D1(d1,None) => {
                        let D1(m,f) = (*d1).deref();
                        Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(4.0),f.clone(),m.clone()],vec![3]))))
                    },
                    V::D2(d2,None) => {
                        let D2(m,f,g) = (*d2).deref();
                        Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(5.0),f.clone(),m.clone(),g.clone()],vec![4]))))
                    },
                    V::Tr2(tr2,None) => {
                        let Tr2(g,h) = (*tr2).deref();
                        Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(2.0),g.clone(),h.clone()],vec![3]))))
                    },
                    V::Tr3(tr3,None) => {
                        let Tr3(f,g,h) = (*tr3).deref();
                        Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(3.0),f.clone(),g.clone(),h.clone()],vec![4]))))
                    },
                    _ => Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(1.0),(&x).as_ref().unwrap().clone()],vec![2])))),
                }
            }
        },
        _ => panic!("illegal decompose arity"),
    };
    //dbg_rtn("decompose",arity,&r);
    r
}

pub fn prim_ind(arity:usize, x: Vn,_w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::BlockInst(_b,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::DervBlockInst(_b,_a,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::Fn(_a,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::R1(_f,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::R2(_f,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::D1(_d1,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::D2(_d2,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::Tr2(_tr2,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            V::Tr3(_tr3,Some(prim)) => Vs::V(V::Scalar(prim as f64)),
            _ => Vs::V(V::Scalar(64 as f64)),
        },
        _ => panic!("illegal plus arity"),
    }
}

pub fn provide() -> A {
    let fns = vec![V::Fn(typ,None),
                   V::Fn(fill,None),
                   V::Fn(log,None),
                   V::Fn(group_len,None),
                   V::Fn(group_ord,None),
                   V::Fn(assert_fn,None),
                   V::Fn(plus,None),
                   V::Fn(minus,None),
                   V::Fn(times,None),
                   V::Fn(divide,None),
                   V::Fn(power,None),
                   V::Fn(floor,None),
                   V::Fn(equals,None),
                   V::Fn(lesseq,None),
                   V::Fn(shape,None),
                   V::Fn(reshape,None),
                   V::Fn(pick,None),
                   V::Fn(windows,None),
                   V::R1(table,None),
                   V::R1(scan,None),
                   V::R2(fill_by,None),
                   V::R2(cases,None),
                   V::R2(catches,None)];
    A::new(fns,vec![23])
}
