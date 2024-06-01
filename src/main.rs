#![feature(allocator_api)]

use std::{thread, time::Duration};

use transpiler::resource::TestSourceCodeProvider;

use crate::transpiler::{transpile, context::{TranspileSettings, TranspileContext}};

pub mod transpiler;
pub mod localize;

fn main() {

    //unsafe { backtrace_on_stack_overflow::enable() };

    let source = 
"
import test::test_module2

class TestClass<T> {
    let field: T?

    function test() -> TestClass<T> {
        return new TestClass {
            field: null
        }
    }
}

let a = new TestClass { field: 100.0 }
let b = a.test().field

let c = if true { 100 } else { error(0) }

let d = if true { ok(100) } else { error(0) }
let e = d!:{ 100 }
let f = error(0)!:{ 100 }


function <T> some(value: T) -> T? {
    return value
}

function <T, E> ok(value: T) -> T!<E> {
    return value
}

function <T, E> error(error: E) -> T!<E> {}


let g: int = new TestStruct { field: 100 }.test1()

struct TestStruct<T> {
    let field: T
}

interface TestInterface<U> {
    function test1() -> U where U: TestInterface3 {}
}

interface TestInterface2<U> {
    function test1() -> U {}
}

interface TestInterface3 {}

implements<T> TestInterface3 for TestStruct<T> {}

implements<T, U> TestInterface<U> for T {
    function test1() -> U where U: TestInterface3 {}
}

implements<T, U> TestInterface2<U> for T {
    function test1() -> U {}
}
";
let source1 = 
"
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
