
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
pub trait Add<Other, Out, > {
    fn add(other: Other, ) -> Out {
    }
}
