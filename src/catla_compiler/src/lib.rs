pub mod compiler;
pub mod resource;
pub mod settings;

#[cfg(test)]
mod test {
    use std::path::Path;

    use catla_import::resource::PackageResourceSet;

    use crate::{compiler::CatlaCompiler, settings::CatlaCompilerSettings};

    #[test]
    fn compiler() {
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "full");
        };

        let mut package_resource_set = PackageResourceSet::new();
        package_resource_set
            .search_source_code("test", Path::new("../../test/src"))
            .unwrap();
        package_resource_set
            .search_source_code("std", Path::new("../../std/src"))
            .unwrap();

        let settings = CatlaCompilerSettings { threads: 20 };

        let compiler = CatlaCompiler::new(package_resource_set, settings);
        compiler.compile();
    }
}
