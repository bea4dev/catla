use core::str;

use crate::memory::CatlaRefObject;

pub fn print(str: &CatlaRefObject<super::string::String>) {
    match &str.value {
        super::string::String::Static(str) => {
            println!("{}", str);
        }
        super::string::String::NonStatic(buffer) => unsafe {
            let buffer = (*buffer.get()).as_ref();
            let str = str::from_utf8_unchecked(buffer);
            println!("{}", str);
        },
    }
}
