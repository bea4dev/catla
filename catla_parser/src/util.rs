
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

    pub(crate) use bump_vec;
}