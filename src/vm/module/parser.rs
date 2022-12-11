use std::{num::{ParseIntError, ParseFloatError}, result};

use regex::Regex;
use thiserror::Error;

use crate::vm::module::const_value::ConstValue;

pub fn parse_module(code: &str) {
    let code_length = code.chars().count();
    let mut current_position: usize = 0;

    let mut const_value_list: Vec<ConstValue> = Vec::new();

    loop {
        let line = read_line(code, code_length, &mut current_position);
        if current_position == code_length - 1 {
            break;
        }
        println!("{}", line);
        match line.as_str() {
            "$const" => {
                let result = parse_const_value(code, code_length, &mut current_position, &mut const_value_list);
                match result {
                    Ok(_) => {},
                    Err(err) => panic!("{:?}", err)
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
}

pub fn read_line(code: &str, code_length: usize, current_position: &mut usize) -> String {
    let mut line: String = String::new();

    for i in *current_position..code_length {
        let char = code.chars().nth(i).unwrap();
        if char == '\n' {
            if i + 1 == code_length {
                *current_position = i;
            } else {
                *current_position = i + 1;
            }
            return line;
        }
        line.push(char);
    }
    *current_position = code_length - 1;

    return line;
}

pub fn read_until(code: &str, code_length: usize, target_char: char, current_position: &mut usize) -> Result<String, ByteCodeParseError> {
    let mut line: String = String::new();

    for i in *current_position..code_length {
        let char = code.chars().nth(i).unwrap();
        if char == target_char {
            *current_position = i;
            return Result::Ok(line);
        }
        line.push(char);
    }

    return Result::Err(ByteCodeParseError::NotFoundCharError(*current_position, target_char));
}

pub fn parse_const_value(code: &str, code_length: usize, current_position: &mut usize, const_value_list: &mut Vec<ConstValue>) -> Result<(), ByteCodeParseError> {
    let info_reg = Regex::new(r"(\d+):(\d+):(\w)").unwrap();
    let mut current_index = 0;

    loop {
        let mut position_temp = *current_position;
        let line = read_line(code, code_length, &mut position_temp);
        if line.as_str() == "$end" {
            *current_position = position_temp;
            return Result::Ok(());
        }

        let info = match read_until(code, code_length, '#', current_position) {
            Ok(info_str) => info_str,
            Err(err) => return Result::Err(err),
        };
        if *current_position + 1 != code_length {
            *current_position += 1;
        }

        let caps = match info_reg.captures(info.as_str()) {
            Some(caps) => caps,
            None => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, "Info not match.".to_string()))
        };

        let index: usize = match (&caps[1]).parse() {
            Ok(index) => index,
            Err(err) => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, err.to_string()))
        };

        if index != current_index {
            return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, "Invalid value index.".to_string()));
        }

        let length: usize = match (&caps[2]).parse() {
            Ok(length) => length,
            Err(_) => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, "Length is not unsigned integer".to_string()))
        };

        let char = match (&caps[3]).chars().nth(0) {
            Some(char) => char,
            None => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, "Invalid type.".to_string()))
        };

        let mut value_string = String::new();
        let position = (*current_position).clone();

        for i in position..(position + length) {
            value_string.push(code.chars().nth(i).unwrap());
        }

        *current_position = position + length;
        read_line(code, code_length, current_position);

        let value = match char {
            'i' => ConstValue::Integer(match value_string.parse::<i64>() 
                {
                    Ok(v) => v,
                    Err(err) => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, err.to_string()))
                }),
            'u' => ConstValue::UnsignedInteger(match value_string.parse::<u64>() 
                {
                    Ok(v) => v,
                    Err(err) => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, err.to_string()))
                }),
            'f' => ConstValue::Float(match value_string.parse::<f64>() 
                {
                    Ok(v) => v,
                    Err(err) => return Result::Err(ByteCodeParseError::InvalidConstValueError(*current_position, err.to_string()))
                }),
            's' => ConstValue::String(value_string),

            _ => return Result::Err(ByteCodeParseError::ConstValueParseError(position))
        };

        const_value_list.push(value);

        current_index += 1;
    }
}


#[derive(Error, Debug)]
pub enum ByteCodeParseError {
    #[error("[position : {0}] | '{1}' is expected, but not fount.")]
    NotFoundCharError(usize, char),

    #[error("[position : {0}] | Invalid const value. => {1}")]
    InvalidConstValueError(usize, String),

    #[error("[position : {0}] | Failed to parse const value.")]
    ConstValueParseError(usize),
}