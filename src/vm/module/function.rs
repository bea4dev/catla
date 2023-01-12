use crate::vm::module::object_type::Type;
use crate::vm::module::order::orders::Order;
use crate::vm::module::parser::{ArgumentTypeInfo, TypeInfo};

#[repr(C)]
pub struct Function {
    pub jit_function_address: usize,
    pub jit_function_bridge_address: usize,

    pub name: String,
    pub register_length: usize,
    pub variable_length: usize,
    pub argument_type_info_list: Vec<ArgumentTypeInfo>,
    pub argument_type_list: Vec<Type>,
    pub return_type_info: TypeInfo,
    pub return_type: Type,
    pub label_block_list: Vec<FunctionLabelBlock>
}

pub struct FunctionLabelBlock {
    pub name: String,
    pub order_list: Vec<Box<dyn Order>>
}