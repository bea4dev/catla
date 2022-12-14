use std::{num::{ParseIntError, ParseFloatError}, result};
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter, write};

use regex::Regex;

use crate::vm::module::const_value::ConstValue;
use crate::vm::module::function::{Function, FunctionLabelBlock};
use crate::vm::module::object_type::Type;
use crate::vm::module::order::get_order::GetArgumentOrder;
use crate::vm::module::order::orders::Order;


#[derive(Clone)]
pub struct FieldInfo {
    field_name: String,
    type_info: TypeInfo
}

#[derive(Clone)]
pub enum TypeInfo {
    PrimitiveType { primitive_type: Type },
    DefinedType { import_module_index: usize, type_name: String }
}

#[derive(Clone)]
pub struct TypeDefineInfo {
    type_name: String,
    field_info_list: Vec<FieldInfo>,
    extends_type_info: Option<TypeInfo>
}



pub fn parse_module(code: &str) {
    let code_length = code.chars().count();
    let mut current_position: usize = 0;
    let mut current_line_index: usize = 0;

    let mut const_value_list: Vec<ConstValue> = Vec::new();
    let mut import_module_name_list: Vec<String> = Vec::new();
    let mut type_define_list: Vec<TypeDefineInfo> = Vec::new();
    let mut using_type_info_list: Vec<TypeInfo> = Vec::new();

    loop {
        let line = read_line(code, code_length, &mut current_position, &mut current_line_index);
        if current_position == code_length - 1 {
            break;
        }
        println!("line : {}", line);
        match line.as_str() {
            "$const" => {
                let result = parse_const_value(code, code_length, &mut current_position, &mut current_line_index);
                const_value_list = match result {
                    Ok(list) => list,
                    Err(err) => panic!("{}", err)
                }
            },
            "$import" => {
                let result = parse_imports(code, code_length, &mut current_position, &mut current_line_index, &const_value_list);
                import_module_name_list = match result {
                    Ok(list) => list,
                    Err(err) => panic!("{}", err)
                }
            },
            "$typedef" => {
                let result = parse_typedef(code, code_length, &mut current_position, &mut current_line_index, &const_value_list);
                type_define_list = match result {
                    Ok(list) => list,
                    Err(err) => panic!("{}", err)
                }
            },
            "$type" => {
                let result = parse_using_types(code, code_length, &mut current_position, &mut current_line_index, &const_value_list);
                using_type_info_list = match result {
                    Ok(list) => list,
                    Err(err) => panic!("{}", err)
                }
            }
            _ => {}
        }
    }

    for value in const_value_list.iter() {
        match value {
            ConstValue::Integer(v) => println!("const i64 : {}", v),
            ConstValue::UnsignedInteger(v) => println!("const u64 : {}", v),
            ConstValue::Float(v) => println!("const f64 : {}", v),
            ConstValue::String(v) => println!("const str : {}", v.clone())
        }
    }

    for module_name in import_module_name_list.iter() {
        println!("import : {}", module_name);
    }

    for type_define_info in type_define_list.iter() {
        let type_define_info: &TypeDefineInfo = type_define_info;
        println!("type : {}", type_define_info.type_name);
        for field_info in (&type_define_info.field_info_list).iter() {
            let field_info: &FieldInfo = field_info;
            println!("field : {}", field_info.field_name);
        }
    }

    for type_info in using_type_info_list.iter() {
        let type_info: &TypeInfo = type_info;
        match type_info {
            TypeInfo::PrimitiveType { primitive_type } => { panic!("PRIMITIVE TYPE!"); }
            TypeInfo::DefinedType { import_module_index, type_name } => println!("using : {}:{}", import_module_index, type_name)
        }
    }
}

pub fn read_line(code: &str, code_length: usize, current_position: &mut usize, current_line_index: &mut usize) -> String {
    let mut line: String = String::new();

    for i in *current_position..code_length {
        let char = code.chars().nth(i).unwrap();
        if char == '\n' {
            if i + 1 == code_length {
                *current_position = i;
            } else {
                *current_position = i + 1;
            }
            *current_line_index += 1;

            return line;
        }
        line.push(char);
    }
    *current_position = code_length - 1;

    return line;
}

pub fn read_until(code: &str, code_length: usize, target_char: char, current_position: &mut usize,
                  current_line_index: &mut usize) -> Result<String, ByteCodeParseError> {
    let mut line: String = String::new();

    for i in *current_position..code_length {
        let char = code.chars().nth(i).unwrap();
        if char == '\n' {
            *current_line_index += 1;
        }
        if char == target_char {
            *current_position = i;
            return Ok(line);
        }
        line.push(char);
    }

    return Err(ByteCodeParseError::NotFoundCharError(*current_position, target_char));
}

pub fn parse_const_value(code: &str, code_length: usize, current_position: &mut usize,
                         current_line_index: &mut usize) -> Result<Vec<ConstValue>, ByteCodeParseError> {
    let info_reg = Regex::new(r"(\d+):(\d+):(\w)").unwrap();
    let mut current_index = 0;
    let mut const_value_list: Vec<ConstValue> = Vec::new();

    loop {
        let mut position_temp = *current_position;
        let mut line_index_temp = *current_line_index;
        let line = read_line(code, code_length, &mut position_temp, &mut line_index_temp);
        if line.as_str() == "$end" {
            *current_position = position_temp;
            *current_line_index = line_index_temp;
            return Ok(const_value_list);
        }

        let current_line = *current_line_index;
        let info = match read_until(code, code_length, '#', current_position, current_line_index) {
            Ok(info_str) => info_str,
            Err(err) => return Err(err),
        };
        if *current_position + 1 != code_length {
            *current_position += 1;
        }

        let caps = match info_reg.captures(info.as_str()) {
            Some(caps) => caps,
            None => return Err(ByteCodeParseError::InvalidConstValueError(current_line, "Info not match.".to_string()))
        };

        let index: usize = match (&caps[1]).parse() {
            Ok(index) => index,
            Err(err) => return Err(ByteCodeParseError::InvalidConstValueError(current_line, err.to_string()))
        };

        if index != current_index {
            return Err(ByteCodeParseError::InvalidConstValueError(current_line, "Invalid value index.".to_string()));
        }

        let length: usize = match (&caps[2]).parse() {
            Ok(length) => length,
            Err(_) => return Err(ByteCodeParseError::InvalidConstValueError(current_line, "Length is not unsigned integer".to_string()))
        };

        let char = match (&caps[3]).chars().nth(0) {
            Some(char) => char,
            None => return Err(ByteCodeParseError::InvalidConstValueError(current_line, "Invalid type.".to_string()))
        };

        let mut value_string = String::new();
        let position = (*current_position).clone();

        for i in position..(position + length) {
            value_string.push(code.chars().nth(i).unwrap());
        }

        *current_position = position + length;
        let current_line = *current_line_index;
        read_line(code, code_length, current_position, current_line_index);

        let value = match char {
            'i' => ConstValue::Integer(match value_string.parse::<i64>() 
                {
                    Ok(v) => v,
                    Err(err) => return Err(ByteCodeParseError::InvalidConstValueError(current_line, err.to_string()))
                }),
            'u' => ConstValue::UnsignedInteger(match value_string.parse::<u64>() 
                {
                    Ok(v) => v,
                    Err(err) => return Err(ByteCodeParseError::InvalidConstValueError(current_line, err.to_string()))
                }),
            'f' => ConstValue::Float(match value_string.parse::<f64>() 
                {
                    Ok(v) => v,
                    Err(err) => return Err(ByteCodeParseError::InvalidConstValueError(current_line, err.to_string()))
                }),
            's' => ConstValue::String(value_string),

            _ => return Err(ByteCodeParseError::ConstValueParseError(current_line))
        };

        const_value_list.push(value);

        current_index += 1;
    }
}

pub fn parse_imports(code: &str, code_length: usize, current_position: &mut usize, current_line_index: &mut usize,
                     const_value_list: &Vec<ConstValue>) -> Result<Vec<String>, ByteCodeParseError> {

    let info_reg = Regex::new(r"(\d+):(.+)").unwrap();
    let mut current_index = 0;
    let mut import_module_name_list: Vec<String> = Vec::new();

    loop {
        let current_line = *current_line_index;
        let line = read_line(code, code_length, current_position, current_line_index);
        if line.as_str() == "$end" {
            return Ok(import_module_name_list);
        }

        let line_str = line.as_str();

        match info_reg.captures(line_str) {
            Some(captures) => {
                let index = (&captures[1]).parse::<usize>().unwrap();
                if index != current_index {
                    return Err(ByteCodeParseError::InvalidImportIndexError(current_line, index));
                }

                let import_string = (&captures[2]).to_string();
                let import_module_name = if import_string.as_str() == "this" {
                    import_string.clone()
                } else {
                    match get_const_value_string(import_string.as_str(), current_line, const_value_list) {
                        Ok(string) => string,
                        Err(err) => { return Err(err) }
                    }
                };

                import_module_name_list.push(import_module_name);
            },
            _ => {
                return Err(ByteCodeParseError::ImportParseError(current_line, line.clone()));
            }
        };

        current_index += 1;
    }
}

pub fn parse_typedef(code: &str, code_length: usize, current_position: &mut usize, current_line_index: &mut usize,
                     const_value_list: &Vec<ConstValue>) -> Result<Vec<TypeDefineInfo>, ByteCodeParseError> {
    let has_extends_reg = Regex::new(r"(.+):\((.+)\):(.+)").unwrap();
    let normal_reg = Regex::new(r"(.+):\((.+)\)").unwrap();
    let field_info_defined_reg = Regex::new(r"(.+):(.+:.+)").unwrap();
    let field_info_primitive_reg = Regex::new(r"(.+):(.+)").unwrap();

    let mut type_define_list: Vec<TypeDefineInfo> = Vec::new();

    loop {
        let current_line = *current_line_index;
        let line = read_line(code, code_length, current_position, current_line_index);
        if line.as_str() == "$end" {
            return Ok(type_define_list);
        }

        let mut name_info: String;
        let mut fields_info: String;
        let mut extends_type_info: Option<TypeInfo>;
        match has_extends_reg.captures(line.as_str()) {
            Some(captures) => {
                name_info = (&captures[1]).to_string();
                fields_info = (&captures[2]).to_string();
                extends_type_info = Some(match parse_type_info(&captures[3], current_line, const_value_list) {
                    Ok(info) => {
                        match &info {
                            TypeInfo::DefinedType { import_module_index, type_name } => info,
                            TypeInfo::PrimitiveType { primitive_type } => { return Err(ByteCodeParseError::InvalidTypeDefineError(current_line, line.clone())); }
                        }
                    },
                    Err(err) => { return Err(err); }
                });
            },
            _ => {
                match normal_reg.captures(line.as_str()) {
                    Some(captures) => {
                        name_info = (&captures[1]).to_string();
                        fields_info = (&captures[2]).to_string();
                        extends_type_info = None;
                    },
                    _ => { return Err(ByteCodeParseError::InvalidTypeDefineError(current_line, line.clone())) }
                }
            }
        }

        let type_name = match get_const_value_string(name_info.as_str(), current_line, const_value_list) {
            Ok(name) => name,
            Err(err) => { return Err(err); }
        };

        let mut field_info_list: Vec<FieldInfo> = Vec::new();
        for field_info_str in fields_info.split(",") {
            match field_info_defined_reg.captures(field_info_str) {
                Some(captures) => {
                    let field_name = match get_const_value_string(&captures[1], current_line, const_value_list) {
                        Ok(name) => name,
                        Err(err) => { return Err(err); }
                    };

                    let type_info = match parse_type_info(&captures[2], current_line, const_value_list) {
                        Ok(info) => info,
                        Err(err) => { return Err(err); }
                    };

                    field_info_list.push(FieldInfo {field_name, type_info});
                },
                _ => {
                    match field_info_primitive_reg.captures(field_info_str) {
                        Some(captures) => {
                            let field_name = match get_const_value_string(&captures[1], current_line, const_value_list) {
                                Ok(name) => name,
                                Err(err) => { return Err(err); }
                            };

                            let type_info = match parse_type_info(&captures[2], current_line, const_value_list) {
                                Ok(field_type) => field_type,
                                Err(err) => { return Err(err); }
                            };

                            field_info_list.push(FieldInfo {field_name, type_info });
                        },
                        _ => { return Err(ByteCodeParseError::InvalidTypeDefineError(current_line, field_info_str.to_string())); }
                    }
                }
            }
        }

        type_define_list.push(TypeDefineInfo {type_name, field_info_list, extends_type_info});
    }
}

pub fn parse_using_types(code: &str, code_length: usize, current_position: &mut usize, current_line_index: &mut usize,
                         const_value_list: &Vec<ConstValue>) -> Result<Vec<TypeInfo>, ByteCodeParseError> {
    let info_reg = Regex::new(r"(\d+):(.+:.+)").unwrap();
    let mut current_index = 0;

    let mut using_type_info_list: Vec<TypeInfo> = Vec::new();

    loop {
        let current_line = *current_line_index;
        let line = read_line(code, code_length, current_position, current_line_index);
        if line.as_str() == "$end" {
            return Ok(using_type_info_list);
        }

        match info_reg.captures(line.as_str()) {
            Some(captures) => {
                let index = match (&captures[1]).parse::<usize>() {
                    Ok(index) => index,
                    Err(_) => { return Err(ByteCodeParseError::InvalidIndexError(current_line, (&captures[1]).to_string())); }
                };
                if current_index != index {
                    return Err(ByteCodeParseError::InvalidIndexError(current_line, (&captures[1]).to_string()));
                }

                let type_info = match parse_type_info(&captures[2], current_line, const_value_list) {
                    Ok(type_info) => type_info,
                    Err(err) => { return Err(err); }
                };

                using_type_info_list.push(type_info);

                current_index += 1;
            },
            _ => { return Err(ByteCodeParseError::UsingTypeParseError(current_line, line.clone())); }
        }
    }
}

pub fn parse_functions(code: &str, code_length: usize, current_position: &mut usize,
                       current_line_index: &mut usize, const_value_list: &Vec<ConstValue>) -> Result<Vec<Function>, ByteCodeParseError> {
    let info_reg = Regex::new(r"(\d+):(.+)\((.+)\)->(.+)\{").unwrap();
    let mut current_index = 0;

    let mut function_list: Vec<Function> = Vec::new();

    loop {
        let current_line = *current_line_index;
        let line = read_line(code, code_length, current_position, current_line_index);
        if line.as_str() == "$end" {
            return Ok(function_list);
        }

        match info_reg.captures(line.as_str()) {
            Some(captures) => {
                let index = match (&captures[1]).parse::<usize>() {
                    Ok(index) => {index},
                    Err(_) => { return Err(ByteCodeParseError::InvalidIndexError(current_line, (&captures[1]).to_string())); }
                };

                if index != current_index {
                    return Err(ByteCodeParseError::InvalidIndexError(current_line, (&captures[1]).to_string()));
                }

                let name = match get_const_value_string(&captures[2], current_line, const_value_list) {
                    Ok(name) => name,
                    Err(err) => { return Err(err); }
                };

                let mut argument_type_info_list: Vec<TypeInfo> = Vec::new();
                if (&captures[3]).is_empty() {
                    for type_info_str in (&captures[3]).split(",") {
                        let type_info = match parse_type_info(type_info_str, current_line, const_value_list) {
                            Ok(info) => info,
                            Err(err) => { return Err(err); }
                        };
                        argument_type_info_list.push(type_info);
                    }
                }

                let return_type_info = match parse_type_info(&captures[4], current_line, const_value_list) {
                    Ok(info) => info,
                    Err(err) => { return Err(err); }
                };

                let mut label_block_list: Vec<FunctionLabelBlock> = match parse_function_label_blocks(code, code_length, current_position, current_line_index, const_value_list) {
                    Ok(list) => list,
                    Err(err) => { return Err(err); }
                };

                function_list.push(Function { name, argument_type_info_list, return_type_info, label_block_list })
            },
            _ => { return Err(ByteCodeParseError::UsingTypeParseError(current_line, line.clone())); }
        }

        current_index += 1;
    }
}

pub fn parse_function_label_blocks(code: &str, code_length: usize, current_position: &mut usize,
                                   current_line_index: &mut usize, const_value_list: &Vec<ConstValue>) -> Result<Vec<FunctionLabelBlock>, ByteCodeParseError> {
    let end_reg = Regex::new(r"^ *}$").unwrap();
    let label_reg = Regex::new(r"label:(.+)").unwrap();

    let mut block_label_list: Vec<FunctionLabelBlock> = Vec::new();

    loop {
        let current_line = *current_line_index;
        let line = read_line(code, code_length, current_position, current_line_index);
        if end_reg.is_match(line.as_str()) {
            return Ok(block_label_list);
        }

        let name = match label_reg.captures(line.as_str()) {
            Some(name) => name,
            _ => { return Err(ByteCodeParseError::ParseFunctionLabelError(current_line, line.to_string())); }
        };



    }
}

pub fn parse_function_orders(code: &str, code_length: usize, current_position: &mut usize,
                             current_line_index: &mut usize, const_value_list: &Vec<ConstValue>) -> Result<Vec<Box<dyn Order>>, ByteCodeParseError> {
    let end_reg = Regex::new(r"^ *label:end$").unwrap();
    let assignment_order_reg = Regex::new(r"(.+)=(.+)").unwrap();

    let mut order_list: Vec<Box<dyn Order>> = Vec::new();

    loop {
        let current_line = *current_line_index;
        let line = read_line(code, code_length, current_position, current_line_index);
        if end_reg.is_match(line.as_str()) {
            return Ok(order_list);
        }

        match assignment_order_reg.captures(line.as_str()) {
            Some(captures) => {
                let assignment_target_index = match parse_register_or_variable_index(&captures[1], current_line) {
                    Ok(target) => target,
                    Err(err) => { return Err(err); }
                };


            },
            _ => {

            }
        }
    }
}

pub fn parse_register_assignment_right_order(code: &str, current_line: usize, target_index: usize) -> Result<Box<dyn Order>, ByteCodeParseError> {
    let mut code_string = code.to_string();
    code_string.retain(|c| c != ' ');

    let code_arguments: Vec<&str> = code_string.split(',').collect();
    let order: dyn Order = match code_arguments.len() {
        2 => {
            match code_arguments[0] {
                "arg" => {
                    let argument_index = match parse_argument_index(code_arguments[1], current_line) {
                        Ok(index) => index,
                        Err(err) => { return Err(err); }
                    };
                    GetArgumentOrder::new(target_index, argument_index);
                },

            }
        }
    };
}


pub fn get_const_value_string(code: &str, line: usize, const_value_list: &Vec<ConstValue>) -> Result<String, ByteCodeParseError> {
    let index = match parse_const_index(code, line) {
        Ok(index) => index,
        Err(err) => { return Err(err); }
    };

    let const_value = match const_value_list.get(index) {
        Some(const_value) => const_value,
        _ => { return Err(ByteCodeParseError::ConstValueNotFoundError(line, index)) }
    };

    return match const_value {
        ConstValue::Integer(_) => Err(ByteCodeParseError::InvalidConstValueTypeError(line, "string".to_string(), "integer".to_string())),
        ConstValue::UnsignedInteger(_) => Err(ByteCodeParseError::InvalidConstValueTypeError(line, "string".to_string(), "unsigned integer".to_string())),
        ConstValue::Float(_) => Err(ByteCodeParseError::InvalidConstValueTypeError(line, "string".to_string(), "integer".to_string())),
        ConstValue::String(string) => Ok(string.clone())
    }
}

pub fn parse_const_index(code: &str, line: usize) -> Result<usize, ByteCodeParseError> {
    let index_reg = Regex::new(r"const#(\d+)").unwrap();
    return match index_reg.captures(code) {
        Some(s) => {
            match (&s[1]).parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::ConstIndexParseError(line, code.to_string()))
            }
        },
        _ => {
            match code.parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::ConstIndexParseError(line, code.to_string()))
            }
        }
    }
}

pub fn parse_register_index(code: &str, line: usize) -> Result<usize, ByteCodeParseError> {
    let index_reg = Regex::new(r"reg#(\d+)").unwrap();
    return match index_reg.captures(code) {
        Some(s) => {
            match (&s[1]).parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::RegisterIndexParseError(line, code.to_string()))
            }
        },
        _ => {
            match code.parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::RegisterIndexParseError(line, code.to_string()))
            }
        }
    }
}

pub fn parse_variable_index(code: &str, line: usize) -> Result<usize, ByteCodeParseError> {
    let index_reg = Regex::new(r"var#(\d+)").unwrap();
    return match index_reg.captures(code) {
        Some(s) => {
            match (&s[1]).parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::VariableIndexParseError(line, code.to_string()))
            }
        },
        _ => {
            match code.parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::VariableIndexParseError(line, code.to_string()))
            }
        }
    }
}

pub fn parse_argument_index(code: &str, line: usize) -> Result<usize, ByteCodeParseError> {
    let index_reg = Regex::new(r"arg#(\d+)").unwrap();
    return match index_reg.captures(code) {
        Some(s) => {
            match (&s[1]).parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::VariableIndexParseError(line, code.to_string()))
            }
        },
        _ => {
            match code.parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::VariableIndexParseError(line, code.to_string()))
            }
        }
    }
}

pub enum RegOrVarIndex {
    RegisterIndex(usize),
    VariableIndex(usize)
}

pub fn parse_register_or_variable_index(code: &str, line: usize) -> Result<RegOrVarIndex, ByteCodeParseError> {
    let register_reg = Regex::new(r"reg#(\d+)").unwrap();
    let variable_reg = Regex::new(r"var#(\d+)").unwrap();
    return match register_reg.captures(code) {
        Some(s) => {
            match (&s[1]).parse::<usize>() {
                Ok(index) => Ok(RegOrVarIndex::RegisterIndex(index)),
                Err(_) => Err(ByteCodeParseError::ParseAssignmentTargetError(line, code.to_string()))
            }
        },
        _ => {
            match variable_reg.captures(code) {
                Some(s) => {
                    match (&s[1]).parse::<usize>() {
                        Ok(index) => Ok(RegOrVarIndex::VariableIndex(index)),
                        Err(_) => Err(ByteCodeParseError::ParseAssignmentTargetError(line, code.to_string()))
                    }
                },
                _ => Err(ByteCodeParseError::ParseAssignmentTargetError(line, code.to_string()))
            }
        }
    }
}

pub fn parse_primitive_type(code: &str, line: usize) -> Result<Type, ByteCodeParseError> {
    return match code {
        "i8" => Ok(Type::I8),
        "i16" => Ok(Type::I16),
        "i32" => Ok(Type::I32),
        "i64" => Ok(Type::I64),
        "u8" => Ok(Type::U8),
        "u16" => Ok(Type::U16),
        "u32" => Ok(Type::U32),
        "u64" => Ok(Type::U64),
        "f32" => Ok(Type::F32),
        "f64" => Ok(Type::F64),
        _ => Err(ByteCodeParseError::PrimitiveTypeParseError(line, code.to_string()))
    }
}

pub fn parse_import_index(code: &str, line: usize) -> Result<usize, ByteCodeParseError> {
    let index_reg = Regex::new(r"import#(\d+)").unwrap();
    return match index_reg.captures(code) {
        Some(s) => Ok((&s[1]).parse::<usize>().unwrap()),
        _ => {
            match code.parse::<usize>() {
                Ok(index) => Ok(index),
                Err(_) => Err(ByteCodeParseError::ImportIndexParseError(line, code.to_string()))
            }
        }
    }
}

pub fn parse_type_info(code: &str, line: usize, const_value_list: &Vec<ConstValue>) -> Result<TypeInfo, ByteCodeParseError> {
    let defined_type_reg = Regex::new(r"(.+):(.+)").unwrap();
    return match defined_type_reg.captures(code) {
        Some(captures) => {
            let import_module_index = match parse_import_index(&captures[1], line) {
                Ok(index) => index,
                Err(err) => { return Err(err); }
            };

            let type_name = match get_const_value_string(&captures[2], line, const_value_list) {
                Ok(name) => name,
                Err(err) => { return Err(err); }
            };

            Ok(TypeInfo::DefinedType { import_module_index, type_name })
        },
        _ => {
            let primitive_type = match parse_primitive_type(code, line) {
                Ok(primitive_type) => primitive_type,
                Err(err) => { return Err(err); }
            };

            Ok(TypeInfo::PrimitiveType { primitive_type })
        }
    }
}


#[derive(Debug)]
pub enum ByteCodeParseError {
    NotFoundCharError(usize, char),
    InvalidConstValueError(usize, String),
    ConstValueParseError(usize),
    ConstIndexParseError(usize, String),
    RegisterIndexParseError(usize, String),
    VariableIndexParseError(usize, String),
    ArgumentIndexParseError(usize, String),
    ParseAssignmentTargetError(usize, String),
    ImportIndexParseError(usize, String),
    ImportParseError(usize, String),
    InvalidImportIndexError(usize, usize),
    ConstValueNotFoundError(usize, usize),
    InvalidConstValueTypeError(usize, String, String),
    InvalidTypeDefineError(usize, String),
    PrimitiveTypeParseError(usize, String),
    UsingTypeParseError(usize, String),
    InvalidIndexError(usize, String),
    InvalidRegVarLengthError(usize, String),
    ParseFunctionLabelError(usize, String)
}

impl Display for ByteCodeParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteCodeParseError::NotFoundCharError(line, char) => write!(f, "{} | '{}' is expected, but not fount.", line, char),
            ByteCodeParseError::InvalidConstValueError(line, string) => write!(f, "{} | Invalid const value. => {}", line, string),
            ByteCodeParseError::ConstValueParseError(line) => write!(f, "{} | Failed to parse const value.", line),
            ByteCodeParseError::ConstIndexParseError(line, string) => write!(f, "{} | Failed to parse const index. => {}", line, string),
            ByteCodeParseError::RegisterIndexParseError(line, string) => write!(f, "{} | Failed to parse register index. => {}", line, string),
            ByteCodeParseError::VariableIndexParseError(line, string) => write!(f, "{} | Failed to parse variable index. => {}", line, string),
            ByteCodeParseError::ArgumentIndexParseError(line, string) => write!(f, "{} | Failed to parse argument index. => {}", line, string),
            ByteCodeParseError::ParseAssignmentTargetError(line, string) => write!(f, "{} | Failed to parse assignment target. => {}", line, string),
            ByteCodeParseError::ImportIndexParseError(line, string) => write!(f, "{} | Failed to parse import index. => {}", line, string),
            ByteCodeParseError::ImportParseError(line, string) => write!(f, "{} | Invalid import. => {}", line, string),
            ByteCodeParseError::InvalidImportIndexError(line, index) => write!(f, "{} | Invalid import index. => {}", line, index),
            ByteCodeParseError::ConstValueNotFoundError(line, index) => write!(f, "{} | Const value is not found. => index:{}", line, index),
            ByteCodeParseError::InvalidConstValueTypeError(line, string1, string2) => write!(f, "{} | Invalid const value type. => Expected '{}', but found '{}'.", line, string1, string2),
            ByteCodeParseError::InvalidTypeDefineError(line, string) => write!(f, "{} | Invalid type define. => {}", line, string),
            ByteCodeParseError::PrimitiveTypeParseError(line, string) => write!(f, "{} | Failed to parse primitive type. => {}", line, string),
            ByteCodeParseError::UsingTypeParseError(line, string) => write!(f, "{} | Failed to parse using type. => {}", line, string),
            ByteCodeParseError::InvalidIndexError(line, string) => write!(f, "{} | Invalid index. => {}", line, string),
            ByteCodeParseError::InvalidRegVarLengthError(line, string) => write!(f, "{} | Invalid length. => {}", line, string),
            ByteCodeParseError::ParseFunctionLabelError(line, string) => write!(f, "{} | Failed to parse function label. => {}", line, string)
        }
    }
}