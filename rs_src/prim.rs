use crate::schema::{A,Vu,Vn,Vs,Decoder};
use cc_mt::Cc;

pub fn plus(arity:usize, x: Vn,w: Vn) -> Vs {
    match arity {
        1 => panic!("no monadic addition"),
        2 => Vs::V(Cc::new(Vu::Scalar(x.unwrap().to_f64() + w.unwrap().to_f64()))),
        _ => panic!("illegal arity"),
    }
}

fn noop(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    Vs::V(Cc::new(Vu::Scalar(0.0)))
}

pub fn provide() -> A {
    let fns = vec![Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop)),Cc::new(Vu::Fn(noop)),
                   Cc::new(Vu::Fn(noop))];
    A::new(fns,vec![Cc::new(Vu::Scalar(23.0))])
}
