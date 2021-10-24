use crate::schema::{A,Vu,Vn,Vs,Decoder};
use cc_mt::Cc;

pub fn plus(arity:usize, x: Vn,w: Vn) -> Vs {
    match arity {
        1 => panic!("no monadic addition"),
        2 => Vs::V(Vu::Scalar(x.unwrap().to_f64() + w.unwrap().to_f64())),
        _ => panic!("illegal arity"),
    }
}

fn noop(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("noop not implemented");
}
fn noop1(_arity: usize, _f: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("noop1 not implemented");
}
fn noop2(_arity: usize, _f: Vn, _g: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("noop2 not implemented");
}


pub fn provide() -> A {
    let fns = vec![Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::Fn(noop),Vu::Fn(noop),
                   Vu::R1(noop1),Vu::R1(noop1),
                   Vu::R2(noop2),Vu::R2(noop2),
                   Vu::R2(noop2)];
    A::new(fns,vec![23])
}
