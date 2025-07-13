use extension_fn::extension_fn;

use crate::{ast::Literal, lexer::Lexer};

#[extension_fn(<'input> Lexer<'input>)]
pub(crate) fn parse_as_literal(&mut self) -> Literal<'input> {
    let token = self.next().unwrap();
    Literal::new(token.text, token.span)
}
