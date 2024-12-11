use std::{
    env::{set_current_dir, set_var},
    path::Path,
    process::Command,
};

use transpiler::{
    context::{AutoImport, OptimizationSettings},
    resource::SourceCodeProvider,
};

use crate::transpiler::{
    context::{TranspileContext, TranspileSettings},
    transpile,
};

pub mod localize;
pub mod transpiler;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    //#[cfg(not(target_os = "windows"))]
    //unsafe { backtrace_on_stack_overflow::enable() };

    let optimization = OptimizationSettings {
        lifetime_analyzer: true,
    };

    let settings = TranspileSettings {
        lang: "ja_JP".to_string(),
        num_threads: num_cpus::get(),
        is_transpiler_debug: true,
        optimization,
        codegen_dir: "./.catla".to_string(),
    };

    let mut resource_provider = SourceCodeProvider::new();
    resource_provider
        .add_entry("std".to_string(), &Path::new("./std/src"))
        .unwrap();
    resource_provider
        .add_entry("test".to_string(), &Path::new("./test/src"))
        .unwrap();

    let mut auto_import = AutoImport::new();
    auto_import.add_module("std::operator::add");
    auto_import.add_module("std::operator::sub");
    auto_import.add_module("std::operator::mul");
    auto_import.add_module("std::operator::div");
    auto_import.add_module("std::compare::equal");
    auto_import.add_module("std::compare::order");
    auto_import.add_module("std::string");
    auto_import.add_element("print", "std::console");

    let context = TranspileContext::new(settings, auto_import, resource_provider);

    transpile("test::main".to_string(), context.clone()).unwrap();

    context.print_report();

    set_current_dir(Path::new(".catla/test")).unwrap();
    set_var("RUSTFLAGS", "-Awarnings");
    Command::new("cargo").arg("run").spawn().unwrap();
}
