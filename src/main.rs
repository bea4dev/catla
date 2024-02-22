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

    let source_code = SourceCode { code: source.to_string(), module_name: "test_module".to_string(), path: None };
    let settings = TranspileSettings {
        lang: "ja_JP".to_string(),
        num_threads: num_cpus::get()
    };
    let context = TranspileContext::new(settings);
    
    transpile(source_code, context.clone());

    context.print_report();

}
