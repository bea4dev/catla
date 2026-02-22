pub mod compiler;
pub mod resource;
pub mod settings;

#[cfg(test)]
mod test {
    use std::{
        path::{Path, PathBuf},
        process::Command,
    };

    use catla_codegen::CodegenSettings;
    use catla_crate::{CrateInfo, CrateInfoSet, Dependency};
    use catla_import::resource::PackageResourceSet;
    use semver::Version;

    use crate::{compiler::CatlaCompiler, settings::CatlaCompilerSettings};

    fn create_test_compiler(out_dir_name: &str) -> (CatlaCompiler, PathBuf) {
        let mut crate_info_set = CrateInfoSet::new();
        crate_info_set.crates.push(CrateInfo {
            name: "std".to_string(),
            version: Version::parse("0.1.0").unwrap(),
            dependencies: Vec::new(),
        });
        crate_info_set.crates.push(CrateInfo {
            name: "test".to_string(),
            version: Version::parse("0.1.0").unwrap(),
            dependencies: vec![Dependency {
                name: "std".to_string(),
                version: Version::parse("0.1.0").unwrap(),
            }],
        });

        let mut package_resource_set = PackageResourceSet::new();
        package_resource_set
            .search_source_code("test", Path::new("../../test/src"))
            .unwrap();
        package_resource_set
            .search_source_code("std", Path::new("../../std/src"))
            .unwrap();

        let settings = CatlaCompilerSettings { threads: 20 };

        let mut out_dir = PathBuf::new();
        out_dir.push("../../");
        out_dir.push(out_dir_name);
        let codegen_settings = CodegenSettings {
            out_dir: out_dir.clone(),
        };

        let compiler = CatlaCompiler::new(
            crate_info_set,
            package_resource_set,
            settings,
            codegen_settings,
        );

        (compiler, out_dir)
    }

    #[test]
    fn compiler() {
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "full");
        };

        let (compiler, out_dir) = create_test_compiler(".catla_compiler_test");
        compiler.compile();

        let mut dir_temp = out_dir.clone();
        dir_temp.push("test");

        Command::new("cargo")
            .arg("run")
            .current_dir(&dir_temp)
            .spawn()
            .unwrap();
    }

    #[test]
    fn optimization_debug() {
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "full");
        };

        let (compiler, _) = create_test_compiler(".catla_optimization_debug");
        compiler.compile();
        compiler.print_optimization_debug();
    }

    #[test]
    fn type_infer_debug() {
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "full");
        };

        let (compiler, _) = create_test_compiler(".catla_type_infer_debug");
        compiler.compile();
        compiler.print_type_infer_debug();
    }
}
