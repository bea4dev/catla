pub mod parser_utils {
    macro_rules! bump_vec {
        ($allocator:expr) => (
            std::vec::Vec::new_in($allocator)
        );
        ($allocator:expr, $($x:expr),*) => (
            if true {
                let mut vec = std::vec::Vec::new_in($allocator);
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
            pub static AST_TYPE_NAME_MAP: std::sync::LazyLock<std::collections::HashMap<std::any::TypeId, &'static str>>
                = std::sync::LazyLock::new(|| {
                    let mut map = std::collections::HashMap::new();
                    $(map.insert(typeid::of::<$x>(), std::any::type_name::<$x>());)*
                    map
            });
        );
    }

    pub(crate) use bump_vec;
    pub(crate) use impl_ast;
}
