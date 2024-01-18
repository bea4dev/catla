use bumpalo::Bump;
use catla_parser::parser::parse_source;

fn main() {
    println!("Hello, world!");

    let source = 
"
let b = if 100 == 100 { null } else { 100 }?.to_string()
";

    let allocator = Bump::new();
    let program = parse_source(source, &allocator);
    dbg!(program);
}
