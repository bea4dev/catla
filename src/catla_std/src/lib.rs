use std::collections::HashMap;

pub mod borrow;
pub mod dispose;
pub mod io;
pub mod object;
pub mod string;

pub fn get_std_map() -> HashMap<&'static str, &'static str> {
    let mut source_map = HashMap::new();

    source_map.insert("std::borrow", include_str!("borrow.rs"));
    source_map.insert("std::dispose", include_str!("dispose.rs"));
    source_map.insert("std::io", include_str!("io.rs"));
    source_map.insert("std::lib", include_str!("lib.rs"));
    source_map.insert("std::object", include_str!("object.rs"));
    source_map.insert("std::string", include_str!("string.rs"));

    source_map
}
