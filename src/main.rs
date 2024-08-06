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

class TestClazz<T> {
    let field: T?

    function test(let this) -> TestClazz<T> {
        return new TestClass {
            field: null
        }
    }

    function a(let this) -> T {}
}

type TestClass<T> = TestClazz<T>

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


interface TestInterface1 {}
interface TestInterface2 {
    function <S> test2(let this) {}
}

implements<T> TestInterface2 for T? where T: TestInterface1 {
    override function <S> test2(let this) {}
}

implements TestInterface2 for int {
    override function <S> test2(let this) {}
}

function <T: TestInterface1> aaa(i: T?) {
    let a = i.test2()
    ccc(i)
}

function <T> ccc(i: T) where T: TestInterface2 {}

function bbb(i: TestInterface2) {
    let b = i.test2()
}

interface Default {
    static function default() -> This {}
}

implements Default for int {
    override static function default() -> This {
        return 0
    }
}

let a = int::default()
a = 100

interface TestInterface3: TestInterface4 {
    static function test3() -> This {
        return This::test4()
    }
}

interface TestInterface4 {
    static function test4() -> This {}
}

function <T: TestInterface3> aaaa() {
    let b = T::test3()
    let c = T::test4()
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
