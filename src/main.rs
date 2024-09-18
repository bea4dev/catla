#![feature(allocator_api)]

use std::path::Path;

use transpiler::{context::AutoImport, resource::DefaultSourceCodeProvider};

use crate::transpiler::{transpile, context::{TranspileSettings, TranspileContext}};

pub mod transpiler;
pub mod localize;

fn main() {

    std::env::set_var("RUST_BACKTRACE", "1");

    //#[cfg(not(target_os = "windows"))]
    //unsafe { backtrace_on_stack_overflow::enable() };

    let settings = TranspileSettings {
        lang: "ja_JP".to_string(),
        num_threads: num_cpus::get(),
        is_debug: true
    };

    let mut resource_provider = DefaultSourceCodeProvider::new();
    resource_provider.add_entry("std".to_string(), &Path::new("./std/src")).unwrap();
    resource_provider.add_entry("test".to_string(), &Path::new("./test/src")).unwrap();

    let mut auto_import = AutoImport::new();
    auto_import.add_module("std::operator::add");
    auto_import.add_module("std::operator::sub");
    auto_import.add_module("std::operator::mul");
    auto_import.add_module("std::operator::div");
    auto_import.add_module("std::compare::equal");
    auto_import.add_module("std::compare::order");

    let context = TranspileContext::new(settings, auto_import, resource_provider);
    
    transpile("test::test".to_string(), context.clone()).unwrap();

    context.print_report();

}
