
pub mod parser_utils {
    macro_rules! bump_vec {
        ($allocator:expr) => (
            bumpalo::collections::Vec::new_in($allocator)
        );
        ($allocator:expr, $($x:expr),*) => (
            if true {
                let mut vec = bumpalo::collections::Vec::new_in($allocator);
                $(vec.push($x);)*
                vec
            } else {
                unreachable!()
            }
        );
    }

    macro_rules! impl_ast {
        ($($x:ty),*) => (
            $(impl AST for $x {})*
        );
    }

    pub(crate) use bump_vec;
    pub(crate) use impl_ast;
}