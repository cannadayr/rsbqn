use crate::schema::{A,V,Vn,Vs,Decoder};
use crate::ebqn::{call};
use cc_mt::Cc;
use std::cmp::max;
use log::{debug, trace, error, log_enabled, info, Level};

fn dbg_args(fun: &str, arity: usize, x: &Vn, w: &Vn) {
    debug!("calling {}/{}",fun,arity);
    match arity {
        1 => {
            debug!("𝕩 = {}",format!("{}",x.clone().unwrap().to_string()));
        },
        2 => {
            debug!("𝕩 = {}",format!("{}",x.clone().unwrap().to_string()));
            debug!("𝕨 = {}",format!("{}",w.clone().unwrap().to_string()));
        },
        _ => ()
    };
}

// Type
fn typ(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap() {
            V::Scalar(_n) => Vs::V(V::Scalar(1.0)),
            V::A(_a) => Vs::V(V::Scalar(0.0)),
            V::Char(_c) => Vs::V(V::Scalar(2.0)),
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
fn log(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("log not implemented");
}
// GroupLen
fn group_len(arity: usize, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => {
            match x.unwrap() {
                V::A(xa) => {
                    let l = xa.r.iter().fold(-1.0, |acc, i| i.to_f64().max(acc));
                    let s = l as usize + 1;
                    let mut r = vec![V::Scalar(0.0);s.clone()];
                    let mut i = 0;
                    while i < xa.r.len() {
                        let e = xa.r[i].to_f64();
                        if (e >= 0.0) {
                            r[e as usize] = V::Scalar(r[e as usize].to_f64() + 1.0)
                        }
                        i += 1;
                    }
                    Vs::V(V::A(Cc::new(A::new(r,vec![s]))))
                },
                _ => panic!("group_len 𝕩 is not an array"),
            }
        },
        _ => panic!("illegal group_len arity"),
    }
}
// GroupOrd
fn group_ord(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("group_ord not implemented");
}
// !
fn assert_fn(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => match x.unwrap().to_f64() {
            1.0 => Vs::V(V::Scalar(1.0)),
            _ => panic!("assert failed"),
        },
        2 => match x.unwrap().to_f64() {
            1.0 => Vs::V(V::Scalar(1.0)),
            _ => panic!("{}",w.unwrap()),
        },
        _ => panic!("illegal assert arity"),
    }
}
// +
pub fn plus(arity:usize, x: Vn,w: Vn) -> Vs {
    match arity {
        1 => Vs::V(x.unwrap()),
        // a proper u32 to char conversions requires the unstable feature 'assoc_char_funcs'
        // https://github.com/rust-lang/rust/issues/71763
        // use u8's for now
        2 => match (x.unwrap(),w.unwrap()) {
                (V::Char(xc),V::Scalar(ws)) if ws >= 0.0 => Vs::V(V::Char(((xc as u8) + (ws as u8)) as char)),
                (V::Scalar(xs),V::Char(wc)) if xs >= 0.0 => Vs::V(V::Char(((wc as u8) + (xs as u8)) as char)),
                (V::Char(xc),V::Scalar(ws)) if ws <  0.0 => Vs::V(V::Char(((xc as u8) - (ws.abs() as u8)) as char)),
                (V::Scalar(xs),V::Char(wc)) if xs <  0.0 => Vs::V(V::Char(((wc as u8) - (xs.abs() as u8)) as char)),
                (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(xs.to_f64() + ws.to_f64())),
                _ => panic!("dyadic plus pattern not found"),
        },
        _ => panic!("illegal plus arity"),
    }
}
// -
fn minus(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => Vs::V(V::Scalar(-1.0 * x.unwrap().to_f64())),
        // a proper u32 to char conversions requires the unstable feature 'assoc_char_funcs'
        // https://github.com/rust-lang/rust/issues/71763
        // use u8's for now
        2 => match (x.unwrap(),w.unwrap()) {
            (V::Scalar(xs),V::Char(wc)) => Vs::V(V::Char(((wc as u8) - (xs as u8)) as char)),
            (V::Char(xc),V::Char(wc)) if (xc as u8) > (wc as u8) => Vs::V(V::Scalar(-1.0*((xc as u8) - (wc as u8)) as f64)),
            (V::Char(xc),V::Char(wc)) => Vs::V(V::Scalar(((wc as u8) - (xc as u8)) as f64)),
            (V::Scalar(xs),V::Scalar(ws)) => Vs::V(V::Scalar(ws.to_f64() - xs.to_f64())),
            _ => panic!("dyadic minus pattern not found"),
        },
        _ => panic!("illegal minus arity"),
    }
}
// ×
fn times(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        2 => Vs::V(V::Scalar(w.unwrap().to_f64() * x.unwrap().to_f64())),
        _ => panic!("illegal times arity"),
    }
}
// ÷
fn divide(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => Vs::V(V::Scalar(1.0 / x.unwrap().to_f64())),
        2 => Vs::V(V::Scalar(w.unwrap().to_f64() / x.unwrap().to_f64())),
        _ => panic!("illegal divide arity"),
    }
}
// ⋆
fn power(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        1 => Vs::V(V::Scalar(x.unwrap().to_f64().exp())),
        2 => Vs::V(V::Scalar(w.unwrap().to_f64().powf(x.unwrap().to_f64()))),
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
    match arity {
        1 => match x.unwrap() {
            V::A(xa) => Vs::V(V::Scalar(xa.sh.len() as i64 as f64)),
            _ => panic!("monadic equals 𝕩 is not an array"),
        },
        2 => match x.unwrap() == w.unwrap() {
            true => Vs::V(V::Scalar(1.0)),
            false => Vs::V(V::Scalar(0.0)),
        },
        _ => panic!("illegal equals arity"),
    }
}
// ≤
fn lesseq(arity: usize, x: Vn, w: Vn) -> Vs {
    match arity {
        2 => {
            let t = typ(1,x.clone(),None).to_ref().to_f64();
            let s = typ(1,w.clone(),None).to_ref().to_f64();
            match t != s {
                true  => Vs::V(V::Scalar((s <= t) as i64 as f64)),
                false => Vs::V(V::Scalar((w.unwrap().to_f64() <= x.unwrap().to_f64()) as i64 as f64)),
            }
        },
        _ => panic!("illegal lesseq arity"),
    }
}
// ≢
fn shape(arity: usize, x: Vn, w: Vn) -> Vs {
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
    match arity {
        2 => {
            match (x.unwrap(),w.unwrap()) {
                (V::A(a),V::Scalar(i)) => Vs::V(a.r[i.to_f64() as i64 as usize].clone()),
                _ => panic!("pick - can't index into non array"),
            }
        },
        _ => panic!("illegal pick arity"),
    }
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
fn scan(arity: usize, f: Vn, x: Vn, _w: Vn) -> Vs {
    match arity {
        1 => {
            match x.unwrap() {
                V::A(a) => {
                    let s = &a.sh;
                    if (s.len()==0) {
                        panic!("scan monadic array rank not at least 1");
                    };
                    let l = a.r.len();
                    let mut r = vec![V::Nothing;l];
                    if (l > 0) {
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
        2 => panic!("dyadic scan"),
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

pub fn provide() -> A {
    let fns = vec![V::Fn(typ),
                   V::Fn(fill),
                   V::Fn(log),
                   V::Fn(group_len),
                   V::Fn(group_ord),
                   V::Fn(assert_fn),
                   V::Fn(plus),
                   V::Fn(minus),
                   V::Fn(times),
                   V::Fn(divide),
                   V::Fn(power),
                   V::Fn(floor),
                   V::Fn(equals),
                   V::Fn(lesseq),
                   V::Fn(shape),
                   V::Fn(reshape),
                   V::Fn(pick),
                   V::Fn(windows),
                   V::R1(table),
                   V::R1(scan),
                   V::R2(fill_by),
                   V::R2(cases),
                   V::R2(catches)];
    A::new(fns,vec![23])
}
