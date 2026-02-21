
use crate::operator::div;
use crate::operator::mul;
use crate::operator::sub;
use crate::operator::add;
use crate::string;
use crate::console::{};
use crate::compare::equal;
use crate::compare::order;
use catla_transpile_std::memory::{ CatlaRefObject, CatlaRefManagement };
use catla_transpile_std::holder::{ CatlaObjectHolder, CatlaObjectDummyHolder };
use catla_transpile_std::holder::Hold;
pub fn print(str: &CatlaRefObject<catla_transpile_std::rust_codegen::string::String>) { 
    catla_transpile_std::rust_codegen::print::print(str);
}