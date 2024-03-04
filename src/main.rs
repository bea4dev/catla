#![feature(allocator_api)]

use std::{thread, time::Duration};

use transpiler::resource::TestSourceCodeProvider;

use crate::transpiler::{SourceCode, transpile, context::{TranspileSettings, TranspileContext}};

pub mod transpiler;
pub mod localize;

fn main() {

    let source = 
"
import test::test_module2

let = function() -> int { return 1 }
|| => { 1 * 1 } = 200

let a = b + 20.5

class TestClass<T: Any> {
    var field0: test_module::TestClass

    test()
}

function test() {}
function test() {}
";
let source1 = 
"
import test::test_module1

let = function() -> int { return 1 }
|| => { 1 * 1 } = 200

let a = b + 20.5

class TestClass<T: Any> {
    var field0: test_module::TestClass

    test()
}

function test() {}
function test() {}
";

    let settings = TranspileSettings {
        lang: "ja_JP".to_string(),
        num_threads: num_cpus::get()
    };

    let mut resource_provider = TestSourceCodeProvider::new();
    resource_provider.insert("test::test_module1".to_string(), source.to_string());
    resource_provider.insert("test::test_module2".to_string(), source1.to_string());

    let context = TranspileContext::new(settings, resource_provider);
    
    transpile("test::test_module1".to_string(), context.clone());

    thread::sleep(Duration::from_secs(1));

    context.print_report();

}
