use cc_mt::{Trace, Tracer, /*collect_cycles*/};
use crate::schema::{Vu,Code,Block,EnvUnboxed};

impl Trace for Vu {
    fn trace(&self, _tracer: &mut Tracer) {
        panic!("clearing V");
    }
}

impl Trace for &Vu {
    fn trace(&self, _tracer: &mut Tracer) {
        panic!("clearing &V");
    }
}

impl Trace for Code {
    fn trace(&self, _tracer: &mut Tracer) {
        panic!("clearing Code");
    }
}

impl Trace for Block {
    fn trace(&self, _tracer: &mut Tracer) {
        panic!("clearing Code");
    }
}

impl Trace for EnvUnboxed {
    fn trace(&self, _tracer: &mut Tracer) {
        panic!("clearing env");
    }
}

