use std::fmt::{Debug,Display,Formatter,Result};
use crate::schema::{V,Vs,Ve,Fn,R1,R2,Stack,A};
use log::{debug, trace, error, log_enabled, info, Level};

pub fn fmt_stack(stack: &mut Stack) -> String {
    stack.s.iter().skip(stack.fp).fold(String::new(), |acc, num| acc + &num.to_string() + ";")
}

pub fn fmt_array(a: &Vec<V>) -> String {
    a.iter().fold(String::new(), |acc, num| acc + &num.to_string() + ",")
}

pub fn fmt_err(a: &Vec<V>) -> String {
    a.iter().fold(String::new(), |acc, num| acc + &num.to_string())
}

pub fn fmt_result(a: &A) -> String {
    let mut acc = String::new();
    for i in 0..a.r.len() {
        acc.push(match &a.r[i] {
            V::Char(c) => *c,
            V::Scalar(0.0) => ' ',
            _ => panic!("cant fmt type"),
        })
    }
    acc
}
pub fn dbg_stack_in(op: &str, pos: usize, args: String, stack: &mut Stack) {
    debug!("{:<22}  in: {}",format!("{:<16} @{}",format!("{} {}",op,args),pos),fmt_stack(stack));
}

pub fn dbg_stack_out(op: &str, pos: usize, stack: &mut Stack) {
    debug!("{:<22} out: {}",format!("{:<16} @{}",format!("{}",op),pos),fmt_stack(stack));
}

impl Display for V {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            V::Scalar(n) => write!(f, "{}", *n),
            V::Char(c) => write!(f, "{}", *c),
            V::BlockInst(_b,_prim) => write!(f,"{}","BlockInst"),
            V::UserMd1(_b,_a,_prim) => write!(f,"{}","UserMd1"),
            V::UserMd2(_b,_a,_prim) => write!(f,"{}","UserMd2"),
            V::Nothing => write!(f,"{}","Nothing"),
            V::A(a) => write!(f,"[{}]",fmt_array(&a.r)),
            V::Fn(_a,_prim) => write!(f,"{}","Fn"),
            V::R1(_f,_prim) => write!(f,"{}","R1"),
            V::R2(_f,_prim) => write!(f,"{}","R2"),
            V::D1(_d1,_prim) => write!(f,"{}","D1"),
            V::D2(_d2,_prim) => write!(f,"{}","D2"),
            V::Tr2(_tr2,_prim) => write!(f,"{}","Tr2"),
            V::Tr3(_tr3,_prim) => write!(f,"{}","Tr3"),
        }
    }
}
impl Display for Vs {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Vs::V(v) => write!(f,"{}",v),
            Vs::Slot(_env,_size) => write!(f,"Slot"),
            Vs::Ar(_ar) => write!(f,"ArrayRef"),
            Vs::Match => panic!("can't fmt Match"),
            Vs::Nothing => panic!("can't fmt Nothing"),
        }
    }
}
impl Display for Ve {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Ve::V(v) => write!(f,"{}",v),
            Ve::S(s) => write!(f,"{}",s),
        }
    }
}

impl Debug for Fn {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self)
    }
}
impl Debug for R1 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self)
    }
}
impl Debug for R2 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self)
    }
}
impl Debug for Ve {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Ve::V(v) => write!(f,"{}",v),
            Ve::S(s) => write!(f,"{}",s),
        }
    }
}
