
use crate::operator::div;
use crate::operator::mul;
use crate::operator::sub;
use crate::operator::add;
use crate::string;
use crate::console::{print};
use crate::compare::equal;
use crate::compare::order;
use catla_transpile_std::memory::{ CatlaRefObject, CatlaRefManagement };
use catla_transpile_std::holder::{ CatlaObjectHolder, CatlaObjectDummyHolder };
use catla_transpile_std::holder::Hold;
pub trait PartialOrder<Other, > {
    fn greater_than(other: Other, ) -> 
    bool {
    }
    fn greater_or_equals(other: Other, ) -> 
    bool {
    }
    fn less_than(other: Other, ) -> 
    bool {
    }
    fn less_or_equals(other: Other, ) -> 
    bool {
    }
}
