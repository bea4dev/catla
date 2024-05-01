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
new test::TestClass {
    field0: 100,
    field1: 200
}

implements <T: TestInterface> TestInterface<T> for test::TestClass<T> {}
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
