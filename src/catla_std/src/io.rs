use crate::string::{String, StringInner};

pub fn print(string: String) {
    let str = match &string.inner {
        StringInner::Static { str } => str,
        StringInner::Heap { string } => string.value().as_str(),
    };
    println!("{}", str);
}
