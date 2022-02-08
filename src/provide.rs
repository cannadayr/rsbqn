use crate::schema::{A,V,Vn,Vs,Ve,Decoder,D1,D2,Tr2,Tr3,Fn,R1,R2,Stack,new_string,new_char};
use crate::vm::{call};
use bacon_rajan_cc::Cc;
use std::cmp::max;
use std::ops::Mul;
use log::{debug, trace, error, log_enabled, info, Level};
use std::iter::FromIterator;
use std::ops::Deref;
use std::char;
use itertools::Itertools;
use num_traits::{cast::FromPrimitive};

// Important!
// unsafe is used in this module to directly dereference values based on the arity
// we are trusting the compiler to call the correct code body depending on its arguments

fn dbg_args(fun: &str, arity: usize, x: &Vn, w: &Vn) {
    match arity {
        1 => {
            info!("calling {}/{}: 𝕩 = {}",fun,arity,format!("{}",x.0.unwrap().to_string()));
        },
        2 => {
            info!("calling {}/{}: 𝕩 = {};𝕨 = {}",fun,arity,format!("{}",x.0.unwrap().to_string()),format!("{}",w.0.unwrap().to_string()));
        },
        _ => ()
    };
}
fn dbg_rtn(fun: &str,arity: usize, r: &Vs) {
    info!("rtn     {}/{}: rtn = {}",fun,arity,r);
}

// Type
pub fn typ(arity: usize, x: Vn, _w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("typ");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(_n) => Ok(Vs::V(V::Scalar(1.0))),
            V::A(_a) => Ok(Vs::V(V::Scalar(0.0))),
            V::Char(_c) => Ok(Vs::V(V::Scalar(2.0))),
            V::UserMd1(_b,_a,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::UserMd2(_b,_a,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::D1(_d1,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::D2(_d2,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::Tr2(_tr3,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::Tr3(_tr3,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::Fn(_fn,_prim) => Ok(Vs::V(V::Scalar(3.0))),
            V::R1(_r1,_prim) => Ok(Vs::V(V::Scalar(4.0))),
            V::R2(_r2,_prim) => Ok(Vs::V(V::Scalar(5.0))),
            V::BlockInst(b,_prim) => Ok(Vs::V(V::Scalar(b.def.typ as f64 + 3.0))),
            V::Nothing => Err(Ve::S("no matching value for typ")),
        },
        _ => panic!("typ arity not implemented"),
    }
}
// Fill
fn fill(arity: usize, x: Vn, _w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("fill");
    match arity {
        1 => Ok(Vs::V(V::Scalar(0.0))),
        2 => Ok(Vs::V(unsafe { x.0.unwrap_unchecked() }.clone() )),
        _ => panic!("illegal fill arity"),
    }
}
// Log
fn log(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("log");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(xs) => Ok(Vs::V(V::Scalar(xs.ln()))),
            _ => Err(Ve::S("monadic log expected number")),
        },
        2 => match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
            (V::Scalar(xs),V::Scalar(ws)) => Ok(Vs::V(V::Scalar(xs.ln() / ws.ln()))),
            _ => Err(Ve::S("dyadic log expected numbers")),
        },
        _ => panic!("illegal power arity"),
    }
}
// GroupLen
fn group_len(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("group_len");
    match arity {
        1 => {
            match unsafe { x.0.unwrap_unchecked() } {
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
                    Ok(Vs::V(V::A(Cc::new(A::new(r.clone(),vec![r.len() as usize])))))
                },
                _ => Err(Ve::S("group_len 𝕩 is not an array")),
            }
        },
        2 => {
            match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
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
                    Ok(Vs::V(V::A(Cc::new(A::new(r.clone(),vec![r.len() as usize])))))
                },
                _ => Err(Ve::S("group_len 𝕩 is not an array")),
            }
        },
        _ => panic!("illegal group_len arity"),
    }
}
// GroupOrd
fn group_ord(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("group_ord");
    match arity {
        2 => {
            match (unsafe { &x.0.unwrap_unchecked() },unsafe { &w.0.unwrap_unchecked() }) {
                (V::A(xa),V::A(wa)) => {
                    let (mut s,l) = wa.r.iter().fold((vec![],0.0), |(mut si,li), v| { si.push(li); (si,li as f64 + v.to_f64()) });
                    let mut r = vec![V::Nothing;l as usize];
                    xa.r.iter().enumerate().for_each(|(i,e)| if e.to_f64() >= 0.0  {
                        r[s[e.to_f64() as usize] as usize] = V::Scalar(i as f64);
                        s[e.to_f64() as usize] += 1.0;
                    });
                    let shape = vec![r.len().clone()];
                    Ok(Vs::V(V::A(Cc::new(A::new(r.clone(),shape)))))
                },
                _ => Err(Ve::S("dyadic group_ord x is not an array")),
            }
        },
        _ => panic!("illegal group_ord arity"),
    }
}
// !
fn assert_fn(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "debug")]
    dbg_args("assert_fn",arity,&x,&w);
    let r =
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() }.as_scalar()  {
            Some(n) if *n == 1.0 => Ok(Vs::V(V::Scalar(1.0))),
            _ => Err(Ve::V(x.0.unwrap().clone())),
        },
        2 => match unsafe { x.0.unwrap_unchecked() }.as_scalar()  {
            Some(n) if *n == 1.0 => Ok(Vs::V(V::Scalar(1.0))),
            _ => {
                //let msg = w.0.unwrap().as_a().unwrap().r.iter().map(|e| match e {
                //    V::Char(c) => *c,
                //    _ => panic!("panic is not a string"),
                //}).collect::<Vec<char>>();
                //Err(String::from_iter(&msg).to_owned())
                Err(Ve::V(w.0.unwrap().clone()))
            },
        },
        _ => panic!("illegal assert arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("assert_fn",arity,&r);
    r
}
// +
pub fn plus(arity:usize, x: Vn,w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("plus");
    #[cfg(feature = "debug")]
    dbg_args("plus",arity,&x,&w);
    let r =
    match arity {
        1 => Ok(Vs::V(unsafe { x.0.unwrap_unchecked() }.clone())),
        2 => match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
                (V::Char(xc),V::Scalar(ws)) if *ws >= 0.0 => Ok(Vs::V(V::Char(char::from_u32(u32::from(*xc) + u32::from_f64(*ws).unwrap()).unwrap()))),
                (V::Scalar(xs),V::Char(wc)) if *xs >= 0.0 => Ok(Vs::V(V::Char(char::from_u32(u32::from(*wc) + u32::from_f64(*xs).unwrap()).unwrap()))),
                (V::Char(xc),V::Scalar(ws)) if *ws <  0.0 => Ok(Vs::V(V::Char(char::from_u32(u32::from(*xc) - u32::from_f64(ws.abs()).unwrap()).unwrap()))),
                (V::Scalar(xs),V::Char(wc)) if *xs <  0.0 => Ok(Vs::V(V::Char(char::from_u32(u32::from(*wc) - u32::from_f64(xs.abs()).unwrap()).unwrap()))),
                (V::Scalar(xs),V::Scalar(ws)) => Ok(Vs::V(V::Scalar(xs + ws))),
                _ => Err(Ve::S("dyadic plus pattern not found")),
        },
        _ => panic!("illegal plus arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("plus",arity,&r);
    r
}
// -
fn minus(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("minus");
    #[cfg(feature = "debug")]
    dbg_args("minus",arity,&x,&w);
    let r =
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(xs) => Ok(Vs::V(V::Scalar(-1.0 * xs))),
            _ => Err(Ve::S("monadic minus expected number")),
        },
        2 => match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
            (V::Scalar(xs),V::Char(wc)) => Ok(Vs::V(V::Char(char::from_u32(u32::from(*wc) - u32::from_f64(*xs).unwrap()).unwrap()))),
            (V::Char(xc),V::Char(wc)) if u32::from(*xc) > u32::from(*wc) => Ok(Vs::V(V::Scalar(-1.0*f64::from(u32::from(*xc) - u32::from(*wc))))),
            (V::Char(xc),V::Char(wc)) => Ok(Vs::V(V::Scalar(f64::from(u32::from(*wc) - u32::from(*xc))))),
            (V::Scalar(xs),V::Scalar(ws)) => Ok(Vs::V(V::Scalar(ws - xs))),
            _ => Err(Ve::S("dyadic minus pattern not found")),
        },
        _ => panic!("illegal minus arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("minus",arity,&r);
    r
}
// ×
fn times(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("times");
    #[cfg(feature = "debug")]
    dbg_args("times",arity,&x,&w);
    match arity {
        2 => match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
            (V::Scalar(xs),V::Scalar(ws)) => Ok(Vs::V(V::Scalar(ws * xs))),
            _ => Err(Ve::S("dyadic times illegal arguments")),
        },
        _ => panic!("illegal times arity"),
    }
}
// ÷
fn divide(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("divide");
    #[cfg(feature = "debug")]
    dbg_args("divide",arity,&x,&w);
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(xs) => Ok(Vs::V(V::Scalar(1.0 / xs))),
            _ => Err(Ve::S("monadic divide expected number")),
        },
        2 => match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
            (V::Scalar(xs),V::Scalar(ws)) => Ok(Vs::V(V::Scalar(ws / xs))),
            _ => Err(Ve::S("dyadic divide expected number")),
        },
        _ => panic!("illegal divide arity"),
    }
}
// ⋆
fn power(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("power");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(xs) => Ok(Vs::V(V::Scalar(xs.exp()))),
            _ => Err(Ve::S("monadic power expected number")),
        },
        2 => match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
            (V::Scalar(xs),V::Scalar(ws)) => Ok(Vs::V(V::Scalar(ws.powf(*xs)))),
            _ => Err(Ve::S("dyadic power expected numbers")),
        },
        _ => panic!("illegal power arity"),
    }
}
// ⌊
fn floor(arity: usize, x: Vn, _w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("floor");
    match arity {
        1|2 => Ok(Vs::V(V::Scalar(unsafe { x.0.unwrap_unchecked() }.to_f64().floor()))),
        _ => panic!("illegal divide arity"),
    }
}
// =
fn equals(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("equals");
    #[cfg(feature = "debug")]
    dbg_args("equals",arity,&x,&w);
    let r =
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::A(xa) => Ok(Vs::V(V::Scalar(xa.sh.len() as i64 as f64))),
            V::Char(_xc) => Ok(Vs::V(V::Scalar(0.0))),
            V::Scalar(_xs) => Ok(Vs::V(V::Scalar(0.0))),
            V::UserMd1(_b,_a,_prim) => Ok(Vs::V(V::Scalar(0.0))),
            V::UserMd2(_b,_a,_prim) => Ok(Vs::V(V::Scalar(0.0))),
            V::D2(_d2,_prim) => Ok(Vs::V(V::Scalar(0.0))),
            _ => Err(Ve::S("monadic equals 𝕩 is not a valid value")),
        },
        2 => match unsafe { x.0.unwrap_unchecked() } == unsafe { w.0.unwrap_unchecked() } {
            true => Ok(Vs::V(V::Scalar(1.0))),
            false => Ok(Vs::V(V::Scalar(0.0))),
        },
        _ => panic!("illegal equals arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("equals",arity,&r);
    r
}
// ≤
fn lesseq(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("lesseq");
    #[cfg(feature = "debug")]
    dbg_args("lesseq",arity,&x,&w);
    let r =
    match arity {
        2 => {
            let t = typ(1,Vn(x.0),Vn(None))?.as_v().unwrap().to_f64();
            let s = typ(1,Vn(w.0),Vn(None))?.as_v().unwrap().to_f64();
            if (&x.0).as_ref().unwrap().is_fn() || (&w.0).as_ref().unwrap().is_fn() {
                Err(Ve::S("cannot compare operations"))
            }
            else {
                match t != s {
                    true  => Ok(Vs::V(V::Scalar((s <= t) as i64 as f64))),
                    false => Ok(Vs::V(V::Scalar((unsafe { w.0.unwrap_unchecked() }.to_f64() <= unsafe { x.0.unwrap_unchecked() }.to_f64()) as i64 as f64))),
                }
            }
        },
        _ => panic!("illegal lesseq arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("lesseq",arity,&r);
    r
}
// ≢
fn shape(arity: usize, x: Vn, _w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("shape");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::A(xa) => {
                let ravel = xa.sh.iter().map(|n| V::Scalar(*n as i64 as f64)).collect::<Vec<V>>();
                let shape = vec![ravel.len()];
                Ok(Vs::V(V::A(Cc::new(A::new(ravel,shape)))))
            },
            _ => Err(Ve::S("shape 𝕩 is not an array")),
        },
        _ => panic!("illegal shape arity"),
    }
}
// ⥊
fn reshape(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("reshape");
    match arity {
        1 => {
            match unsafe { x.0.unwrap_unchecked() } {
                V::A(xa) => Ok(Vs::V(V::A(Cc::new(A::new(xa.r.clone(),vec![xa.r.len()]))))),
                _ => Err(Ve::S("monadic reshape no arr")),
            }
        },
        2 => {
            match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
                (V::A(ax),V::A(aw)) => {
                    let sh = aw.r.iter().map(|e| match e {
                        V::Scalar(n) => *n as usize,
                        _ => panic!("W ravel is not a num"),
                    }).collect::<Vec<usize>>();
                    Ok(Vs::V(V::A(Cc::new(A::new(ax.r.clone(),sh)))))
                },
                _ => Err(Ve::S("dydic reshape no match")),
            }
        },
        _ => panic!("illegal reshape arity"),
    }
}
// ⊑
fn pick(arity: usize, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("pick");
    #[cfg(feature = "debug")]
    dbg_args("pick",arity,&x,&w);
    let r =
    match arity {
        2 => {
            match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
                (V::A(a),V::Scalar(i)) if *i >= 0.0 => Ok(Vs::V(a.r[*i as i64 as usize].clone())),
                (V::A(a),V::Scalar(i)) if *i <  0.0 => Ok(Vs::V(a.r[((a.r.len() as f64) + i) as i64 as usize].clone())),
                _ => Err(Ve::S("pick - can't index into non array")),
            }
        },
        _ => panic!("illegal pick arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("pick",arity,&r);
    r
}
// ↕
fn windows(arity: usize, x: Vn, _w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("windows");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(n) => Ok(Vs::V(V::A(Cc::new(A::new((0..*n as i64).map(|v| V::Scalar(v as f64)).collect::<Vec<V>>(),vec![*n as usize]))))),
            _ => Err(Ve::S("x is not a number")),
        },
        _ => panic!("illegal windows arity"),
    }

}
// ⌜
fn table(stack: &mut Stack,arity: usize, f: Vn, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("table");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::A(xa) => {
                let mut ravel: Vec<V> = Vec::with_capacity(xa.r.len());
                for i in 0..xa.r.len() {
                    ravel.push(match call(stack,arity,Vn(f.0),Vn(Some(&xa.r[i])),Vn(None)) {
                        Ok(r) => r.into_v().unwrap(),
                        Err(e) => return Err(e),
                    })
                }
                let sh = (*xa).sh.clone();
                Ok(Vs::V(V::A(Cc::new(A::new(ravel,sh)))))
            },
            _ => Err(Ve::S("monadic table x is not an array")),
        },
        2 => {
            match (unsafe { x.0.unwrap_unchecked() },unsafe { w.0.unwrap_unchecked() }) {
                (V::A(xa),V::A(wa)) => {
                    let mut ravel: Vec<V> = Vec::with_capacity(xa.r.len().mul(wa.r.len()));
                    for i in 0..wa.r.len() {
                        for j in 0..xa.r.len() {
                            ravel.push(match call(stack,arity,Vn(f.0),Vn(Some(&xa.r[j])),Vn(Some(&wa.r[i]))) {
                                Ok(r) => r.into_v().unwrap(),
                                Err(e) => return Err(e),
                            })
                        }
                    }
                    let sh = (*wa).sh.clone().into_iter().chain((*xa).sh.clone().into_iter()).collect();
                    Ok(Vs::V(V::A(Cc::new(A::new(ravel,sh)))))
                },
                _ => Err(Ve::S("dyadic table not an array")),
            }
        },
        _ => panic!("illegal table arity"),
    }
}
// `
fn scan(stack: &mut Stack,arity: usize, f: Vn, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("scan");
    match arity {
        1 => {
            match unsafe { x.0.unwrap_unchecked() } {
                V::A(a) => {
                    let s = &a.sh;
                    if s.len()==0 {
                        return Err(Ve::S("scan monadic array rank not at least 1"));
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
                            r[i] = match call(stack,2,Vn(f.0),Vn(Some(&a.r[i])),Vn(Some(&r[i-c]))) { Ok(v) => v.as_v().unwrap().clone(),Err(e) => panic!("monadic scan call failed"), };
                            i += 1;
                        }
                    };
                    Ok(Vs::V(V::A(Cc::new(A::new(r,s.to_vec())))))
                },
                _ => Err(Ve::S("monadic scan x is not an array")),
            }
        },
        2 => {
            let (wr,wa) = match unsafe { w.0.unwrap_unchecked() } {
                V::A(wa) => (wa.sh.len(),wa.clone()),
                // TODO `wa` doesn't actually need to be a ref counted array
                V::Scalar(ws) => (0,Cc::new(A::new(vec![V::Scalar(*ws)],vec![1]))),
                _ => return Err(Ve::S("dyadic scan w is invalid type")),
            };
            match unsafe { x.0.unwrap_unchecked() } {
                V::A(xa) => {
                    let s = &xa.sh;
                    if s.len()==0 {
                        return Err(Ve::S("scan dyadic array rank not at least 1"));
                    };
                    if 1+wr != s.len() {
                        return Err(Ve::S("scan dyadic array rank don't match"));
                    }
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
                            r[i] = match call(stack,2,Vn(f.0),Vn(Some(&xa.r[i])),Vn(Some(&wa.r[i]))) { Ok(v) => v.as_v().unwrap().clone(), Err(e) => panic!("dyadic scan call failed"),};
                            i += 1;
                        }
                        while i < l {
                            r[i] = match call(stack,2,Vn(f.0),Vn(Some(&xa.r[i])),Vn(Some(&r[i-c]))) { Ok(v) => v.as_v().unwrap().clone(),Err(e) => panic!("dyadic scan call failed"), };
                            i += 1;
                        }
                    };
                    Ok(Vs::V(V::A(Cc::new(A::new(r,s.to_vec())))))
                },
                _ => Err(Ve::S("dyadic scan x or w is not an array")),
            }
        },
        _ => panic!("illegal scan arity"),
    }
}
// _fillBy_
fn fill_by(stack:&mut Stack,arity: usize, f: Vn, _g: Vn, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("fill_by");
    call(stack,arity,f,x,w)
}
// ⊘
fn cases(stack:&mut Stack,arity: usize, f: Vn, g: Vn, x: Vn, w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("cases");
    match arity {
        1 => call(stack,arity,f,x,Vn(None)),
        2 => call(stack,arity,g,x,w),
        _ => panic!("illegal cases arity"),
    }
}
// ⎊
fn catches(_stack:&mut Stack,_arity: usize, _f: Vn, _g: Vn, _x: Vn, _w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("catches");
    panic!("catches not implemented");
}

pub fn decompose(arity:usize, x: Vn,_w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("decompose");
    #[cfg(feature = "debug")]
    dbg_args("decompose",arity,&x,&w);
    let r =
    match arity {
        1 => {
            if // atoms
                match unsafe { (&x).0.as_ref().unwrap_unchecked() } {
                    V::Scalar(_n) => true,
                    V::Char(_c) => true,
                    V::Nothing => true,
                    V::A(_a) => true,
                    _ => false
                }
            {
                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(-1.0),unsafe { x.0.unwrap_unchecked() }.clone()],vec![2]))))
            }
            else if // primitives
                match unsafe { (&x).0.as_ref().unwrap_unchecked() } {
                    V::BlockInst(_b,Some(_prim)) => true,
                    V::UserMd1(_b,_a,Some(_prim)) => true,
                    V::UserMd2(_b,_a,Some(_prim)) => true,
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
                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(0.0),unsafe { x.0.unwrap_unchecked() }.clone()],vec![2]))))
            }
            else if // repr
                match unsafe { (&x).0.as_ref().unwrap_unchecked() } {
                    V::UserMd1(_b,_a,None) => true,
                    V::UserMd2(_b,_a,None) => true,
                    _ => false,
                }
            {
                match unsafe { (&x).0.as_ref().unwrap_unchecked() } {
                    V::UserMd1(b,a,None) => {
                        let t = 3 + b.def.typ;
                        match t {
                            4 => {
                                let D1(f,g) = a.deref();
                                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(4.0),g.clone(),f.clone()],vec![3]))))
                            },
                            _ => return Err(Ve::S("UserMd1 illegal decompose")),
                        }
                    },
                    V::UserMd2(b,a,None) => {
                        let t = 3 + b.def.typ;
                        match t {
                            5 => {
                                let D2(f,g,h) = a.deref();
                                Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(5.0),g.clone(),f.clone(),h.clone()],vec![4]))))
                            },
                            _ => return Err(Ve::S("UserMd2 illegal decompose")),
                        }
                    },
                    _ => panic!("decompose other"),
                }
            }
            else { // everything else
                match unsafe { (&x).0.as_ref().unwrap_unchecked() } {
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
                    _ => Vs::V(V::A(Cc::new(A::new(vec![V::Scalar(1.0),x.0.unwrap().clone()],vec![2])))),
                }
            }
        },
        _ => panic!("illegal decompose arity"),
    };
    #[cfg(feature = "debug")]
    dbg_rtn("decompose",arity,&r);
    Ok(r)
}

pub fn prim_ind(arity:usize, x: Vn,_w: Vn) -> Result<Vs,Ve> {
    #[cfg(feature = "coz-fns")]
    coz::scope!("prim_ind");
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::BlockInst(_b,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::UserMd1(_b,_a,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::UserMd2(_b,_a,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::Fn(_a,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::R1(_f,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::R2(_f,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::D1(_d1,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::D2(_d2,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::Tr2(_tr2,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            V::Tr3(_tr3,Some(prim)) => Ok(Vs::V(V::Scalar(*prim as f64))),
            _ => Ok(Vs::V(V::Scalar(64 as f64))),
        },
        _ => panic!("illegal plus arity"),
    }
}

pub fn glyph(arity:usize, x: Vn,w: Vn) -> Result<Vs,Ve> {
    let glyphs = "+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!˙˜˘¨⌜⁼´˝`∘○⊸⟜⌾⊘◶⎉⚇⍟⎊";
    let prim =
        match x.0.unwrap() {
            V::BlockInst(_b,Some(prim)) => *prim,
            V::UserMd1(_b,_a,Some(prim)) => *prim,
            V::UserMd2(_b,_a,Some(prim)) => *prim,
            V::Fn(_a,Some(prim)) => *prim,
            V::R1(_f,Some(prim)) => *prim,
            V::R2(_f,Some(prim)) => *prim,
            V::D1(_d1,Some(prim)) => *prim,
            V::D2(_d2,Some(prim)) => *prim,
            V::Tr2(_tr2,Some(prim)) => *prim,
            V::Tr3(_tr3,Some(prim)) => *prim,
            _ => panic!("something went wrong in glyph"),
        };
    Ok(Vs::V(new_char(glyphs.chars().nth(prim).unwrap())))
}

pub fn fmtnum(arity:usize, x: Vn,w: Vn) -> Result<Vs,Ve> {
    match arity {
        1 => match unsafe { x.0.unwrap_unchecked() } {
            V::Scalar(n) => Ok(Vs::V(new_string(&*format!("{}",*n)))),
            _ => Err(Ve::S("no matching value for fmtnum")),
        },
        _ => panic!("typ arity not implemented"),
    }
}

pub fn provide() -> A {
    let fns = vec![V::Fn(Fn(typ),None),
                   V::Fn(Fn(fill),None),
                   V::Fn(Fn(log),None),
                   V::Fn(Fn(group_len),None),
                   V::Fn(Fn(group_ord),None),
                   V::Fn(Fn(assert_fn),None),
                   V::Fn(Fn(plus),None),
                   V::Fn(Fn(minus),None),
                   V::Fn(Fn(times),None),
                   V::Fn(Fn(divide),None),
                   V::Fn(Fn(power),None),
                   V::Fn(Fn(floor),None),
                   V::Fn(Fn(equals),None),
                   V::Fn(Fn(lesseq),None),
                   V::Fn(Fn(shape),None),
                   V::Fn(Fn(reshape),None),
                   V::Fn(Fn(pick),None),
                   V::Fn(Fn(windows),None),
                   V::R1(R1(table),None),
                   V::R1(R1(scan),None),
                   V::R2(R2(fill_by),None),
                   V::R2(R2(cases),None),
                   V::R2(R2(catches),None)];
    A::new(fns,vec![23])
}
