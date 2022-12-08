use std::sync::{Arc, Mutex};
use crate::{ObjectType, vm};
use crate::vm::module::const_value::ConstValue;
use crate::vm::module::function::Function;

pub struct Module {
    pub const_value_list: Vec<ConstValue>,
    pub import_module_list: Vec<*mut Module>,
    pub defined_type_list: Vec<*mut ObjectType>,
    pub using_type_list: Vec<*mut ObjectType>,
    pub function_list: Vec<*mut Function>

}