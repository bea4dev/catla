use std::cell::RefCell;
use crate::vm::module::order::orders::Order;
use crate::vm::module::parser::{TypeInfo};

pub struct Function {
    pub name: String,
    pub register_length: usize,
    pub variable_length: usize,
    pub argument_type_info_list: Vec<TypeInfo>,
    pub return_type_info: TypeInfo,
    pub label_block_list: Vec<FunctionLabelBlock>
}

pub struct FunctionLabelBlock {
    pub name: String,
    pub order_list: Vec<Box<dyn Order>>
}