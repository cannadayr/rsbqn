use crate::schema::{A,V,Vn,Vs,Decoder};
use cc_mt::Cc;

pub fn plus(arity:usize, x: Vn,w: Vn) -> Vs {
    match arity {
        1 => panic!("no monadic addition"),
        2 => Vs::V(V::Scalar(x.unwrap().to_f64() + w.unwrap().to_f64())),
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
    let fns = vec![V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::Fn(noop),V::Fn(noop),
                   V::R1(noop1),V::R1(noop1),
                   V::R2(noop2),V::R2(noop2),
                   V::R2(noop2)];
    A::new(fns,vec![23])
}
