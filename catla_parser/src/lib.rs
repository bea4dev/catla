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
let b = if 100 == 100 { null } else { 100 }?.to_string()
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
