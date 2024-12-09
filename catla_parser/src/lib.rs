pub mod lexer;
pub mod parser;
pub mod grammar;

#[test]
fn test() {
    use bumpalo::Bump;
    use crate::parser::parse_source;

    let source = 
"
#[rust_codegen]
function print(String str);
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
