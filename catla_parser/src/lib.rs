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
function <T> test(value: T) -> T where T: TestInterface {
    return value
}

test:<int>(100)
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
