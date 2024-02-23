use transpiler::resource::TestSourceCodeProvider;

use crate::transpiler::{SourceCode, transpile, context::{TranspileSettings, TranspileContext}};

pub mod transpiler;
pub mod localize;

fn main() {

    let source = 
"
let = function() -> int { return 1 }
|| => { 1 * 1 } = 200

let a = b + 20.5

class TestClass<T: Any> {
    var field0: int

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
    resource_provider.insert("test_module".to_string(), source.to_string());

    let context = TranspileContext::new(settings, resource_provider);
    
    transpile("test_module".to_string(), context.clone());

    context.print_report();

}
