use std::fmt::{Display,Formatter,Result};
use crate::schema::{V,Vs};
use log::{debug, trace, error, log_enabled, info, Level};

pub fn fmt_stack(stack: &Vec<Vs>) -> String {
    stack.iter().fold(String::new(), |acc, num| acc + &num.to_string() + ";")
}

pub fn fmt_array(a: &Vec<V>) -> String {
    a.iter().fold(String::new(), |acc, num| acc + &num.to_string() + ",")
}

pub fn dbg_stack_in(op: &str, pos: usize, args: String, stack: &Vec<Vs>) {
    debug!("{:<22}  in: {}",format!("{:<16} @{}",format!("{} {}",op,args),pos),fmt_stack(&stack));
}

pub fn dbg_stack_out(op: &str, pos: usize, stack: &Vec<Vs>) {
    debug!("{:<22} out: {}",format!("{:<16} @{}",format!("{}",op),pos),fmt_stack(&stack));
}

impl Display for V {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            V::Scalar(n) => write!(f, "{}", *n),
            V::Char(c) => write!(f, "{}", *c),
            V::BlockInst(_b) => write!(f,"{}","BlockInst"),
            V::DervBlockInst(_b,_a,_prim) => write!(f,"{}","DervBlockInst"),
            V::Nothing => write!(f,"{}","Nothing"),
            V::A(a) => write!(f,"[{}]",fmt_array(&a.r)),
            V::Fn(_a) => write!(f,"{}","Fn"),
            V::R1(_f) => write!(f,"{}","R1"),
            V::R2(_f) => write!(f,"{}","R2"),
            V::D1(_d1) => write!(f,"{}","D1"),
            V::D2(_d2) => write!(f,"{}","D2"),
            V::Tr2(_tr2) => write!(f,"{}","Tr2"),
            V::Tr3(_tr3) => write!(f,"{}","Tr3"),
        }
    }
}
impl Display for Vs {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Vs::V(v) => write!(f,"{}",v),
            Vs::Slot(_env,_size) => write!(f,"Slot"),
            Vs::Ar(_ar) => write!(f,"ArrayRef")
        }
    }
}

