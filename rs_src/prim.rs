use crate::schema::{A,Vu};
use cc_mt::Cc;
fn noop() { () }
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
