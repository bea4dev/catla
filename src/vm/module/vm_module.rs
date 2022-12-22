use std::collections::HashMap;
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
    pub defined_type_map: HashMap<String, *mut ObjectType>,
    pub using_type_info_list: Vec<TypeInfo>,
    pub using_type_list: Vec<*mut ObjectType>,
    pub function_map: HashMap<String, Function>
}

impl Module {
    pub fn get_function_ptr(&mut self, name: &String) -> Option<*mut Function> {
        return match self.function_map.get_mut(name) {
            Some(function) => Some(function as *mut Function),
            _ => None
        }
    }
}