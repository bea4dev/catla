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
interface Interface where This: Interface1 {}

let array: [[Interface?]] = new { 0, 100, 200 }

function test();
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
