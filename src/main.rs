use std::{collections::HashMap, sync::Mutex};

use crate::transpiler::{SourceCode, transpile, context::{TranspileSettings, TranspileContext}};

pub mod transpiler;
pub mod localize;

fn main() {

    let source = 
"
let = function() -> int { return 1 }
|| => { 1 * 1 } = 200

let a = b + 20.5

class TestClass {
    var field0: int

    test()
}

function test() {}
function test() {}
";

    let source_code = SourceCode { code: source.to_string(), module_name: "test_module".to_string(), path: None };
    let context = TranspileContext::new(TranspileSettings { lang: "ja_JP".to_string() });
    let result = transpile(source_code, context.clone());

    for error in result.errors.iter() {
        error.0.print(&result.module_context);
        print!("\n\n");
    }

    for warning in result.warnings.iter() {
        warning.0.print(&result.module_context);
        print!("\n\n");
    }

}
