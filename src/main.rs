#![feature(allocator_api)]

use std::{path::Path, thread, time::Duration};

use transpiler::{context::AutoImport, resource::DefaultSourceCodeProvider};

use crate::transpiler::{transpile, context::{TranspileSettings, TranspileContext}};

pub mod transpiler;
pub mod localize;

fn main() {

    unsafe { backtrace_on_stack_overflow::enable() };

    let settings = TranspileSettings {
        lang: "ja_JP".to_string(),
        num_threads: num_cpus::get()
    };

    let mut resource_provider = DefaultSourceCodeProvider::new();
    resource_provider.add_entry("std".to_string(), &Path::new("./std/src")).unwrap();
    resource_provider.add_entry("test".to_string(), &Path::new("./test/src")).unwrap();

    let mut auto_import = AutoImport::new();
    auto_import.add_module("std::operators::add");
    auto_import.add_module("std::operators::sub");
    auto_import.add_module("std::operators::mul");
    auto_import.add_module("std::operators::div");

    let context = TranspileContext::new(settings, auto_import, resource_provider);
    
    transpile("test::test".to_string(), context.clone()).unwrap();

    thread::sleep(Duration::from_secs(1));

    context.print_report();

}