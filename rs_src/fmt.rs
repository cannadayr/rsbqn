use std::fmt::{Display,Formatter,Result};
use crate::schema::{V,Vs};

pub fn fmt_stack(stack: &Vec<Vs>) -> String {
    stack.iter().fold(String::new(), |acc, num| acc + &num.to_string() + "⋄")
}

impl Display for V {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            V::Scalar(n) => write!(f, "{}", *n),
            V::BlockInst(_b) => write!(f,"{}","BlockInst"),
            V::DervBlockInst(_b,_a) => write!(f,"{}","DervBlockInst"),
            V::Nothing => write!(f,"{}","Nothing"),
            V::A(_a) => write!(f,"{}","A"),
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

