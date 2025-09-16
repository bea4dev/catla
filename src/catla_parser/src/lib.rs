use std::{mem::transmute, sync::Arc};

use bumpalo::Bump;

use crate::{ast::Program, error::ParseError, lexer::Lexer, parser::parse_program};

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct CatlaAST {
    /// fake static
    ast: &'static Program<'static, 'static>,
    pub source_code: Arc<String>,
    pub source_code_name: Arc<String>,
    pub errors: Arc<std::vec::Vec<ParseError>>,
    _allocator: Arc<Bump>,
}

unsafe impl Send for CatlaAST {}
unsafe impl Sync for CatlaAST {}

impl CatlaAST {
    pub fn parse(
        source_code: impl Into<Arc<String>>,
        source_code_name: impl Into<Arc<String>>,
    ) -> Self {
        let source_code: Arc<String> = source_code.into();
        let source_code_name: Arc<String> = source_code_name.into();

        let mut lexer = Lexer::new(source_code.as_str());
        let allocator = Arc::new(Bump::new());
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &[], &mut errors, allocator.as_ref());

        let fake_static_ast = unsafe { transmute(ast) };

        Self {
            ast: fake_static_ast,
            source_code,
            source_code_name,
            errors: Arc::new(errors),
            _allocator: allocator,
        }
    }

    pub fn ast<'this>(&'this self) -> &'this Program<'this, 'this> {
        self.ast
    }
}

#[cfg(test)]
mod test {
    use bumpalo::Bump;

    use crate::{lexer::Lexer, parser::parse_program};

    #[test]
    fn parser() {
        let source = r#"
function test() -> int {
    return 100
}
"#;

        let mut lexer = Lexer::new(source);
        let allocator = Bump::new();
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &[], &mut errors, &allocator);

        dbg!(ast);
        dbg!(errors);
    }
}
