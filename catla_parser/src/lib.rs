pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod test {
    use bumpalo::Bump;

    use crate::{
        lexer::{Lexer, TokenKind},
        parser::parse_program,
    };

    #[test]
    fn parser() {
        let source = r#"
loop {
    new Test {}
}
        "#;

        let mut lexer = Lexer::new(source);
        let allocator = Bump::new();
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &[TokenKind::None], &mut errors, &allocator);

        dbg!(ast);
        dbg!(errors);
    }
}
