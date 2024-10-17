pub mod lexer;
pub mod parser;
pub mod grammar;

#[test]
fn test() {
    use bumpalo::Bump;
    use crate::parser::parse_source;

    let source = 
"
let (value1, value2): (int, float) = (100, 100.0)
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
