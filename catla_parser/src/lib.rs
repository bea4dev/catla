#![feature(allocator_api)]

pub mod lexer;
pub mod parser;
pub mod util;
pub mod grammar;

#[test]
fn test() {
    use bumpalo::Bump;
    use crate::parser::parse_source;

    let source = 
"
function <T: Any + Map<int, String>> test() {}
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
