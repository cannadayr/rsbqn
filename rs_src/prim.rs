use crate::schema::{A,V,Vn,Vs,Decoder};
use cc_mt::Cc;

/*
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
*/

// Type
fn typ(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("typ not implemented");
}
// Fill
fn fill(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("fill not implemented");
}
// Log
fn log(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("log not implemented");
}
// GroupLen
fn group_len(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("group_len not implemented");
}
// GroupOrd
fn group_ord(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("group_ord not implemented");
}
// !
fn assert_fn(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("assert_fn not implemented");
}
// +
fn plus(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("plus not implemented");
}
// -
fn minus(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("minus not implemented");
}
// ×
fn times(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("times not implemented");
}
// ÷
fn divide(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("divide not implemented");
}
// ⋆
fn power(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("power not implemented");
}
// ⌊
fn floor(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("floor not implemented");
}
// =
fn equals(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("equals not implemented");
}
// ≤
fn lesseq(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("lesseq not implemented");
}
// ≢
fn shape(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("shape not implemented");
}
// ⥊
fn reshape(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("reshape not implemented");
}
// ⊑
fn pick(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("pick not implemented");
}
// ↕
fn windows(_arity: usize, _x: Vn, _w: Vn) -> Vs {
    panic!("windows not implemented");
}
// ⌜
fn table(_arity: usize, _f: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("table not implemented");
}
// `
fn scan(_arity: usize, _f: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("scan not implemented");
}
// _fillBy_
fn fill_by(_arity: usize, _f: Vn, _g: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("fill_by not implemented");
}
// ⊘
fn cases(_arity: usize, _f: Vn, _g: Vn, _x: Vn, _w: Vn) -> Vs {
    panic!("cases not implemented");
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
