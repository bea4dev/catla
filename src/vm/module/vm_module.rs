use std::sync::{Arc, Mutex};
use crate::{ObjectType, vm};
use crate::vm::module::const_value::ConstValue;
use crate::vm::module::function::Function;
use crate::vm::module::parser::{TypeDefineInfo, TypeInfo};

pub struct Module {
    pub is_initialized: bool,

    pub const_value_list: Vec<ConstValue>,
    pub import_module_name_list: Vec<String>,
    pub import_module_list: Vec<*mut Module>,
    pub defined_type_info_list: Vec<TypeDefineInfo>,
    pub defined_type_list: Vec<*mut ObjectType>,
    pub using_type_info_list: Vec<TypeInfo>,
    pub using_type_list: Vec<*mut ObjectType>,
    pub function_list: Vec<Function>
}