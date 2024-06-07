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

    function test() -> TestClazz<T> {
        return new TestClass {
            field: null
        }
    }

    function a() -> T {}
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


interface Nat {}

struct Zero {}
implements Nat for Zero {}

struct Succ<N: Nat> {
    let n: N
}
implements<N: Nat> Nat for Succ<N> {}

interface Add<R: Nat, Answer: Nat> {
    function add(r: R) -> Answer {}
}

implements<N: Nat> Add<N, N> for Zero {
    function add(r: N) -> N {}
}

implements<N: Nat, M: Nat, Sum: Nat> Add<N, Succ<Sum>> for Succ<M> where N: Add<M, Sum> {
    function add(r: N) -> Succ<Sum> {}
}

type One = Succ<Zero>
type Two = Succ<One>
type Three = Succ<Two>
type Four = Succ<Three>

let zero = new Zero {}
let one = new One { n: zero }
let two = new Two { n: one }

let three: Three = one.add(two)
let four: Four = two.add(two)

let incorrect: Four = three.add(two)

";
let source1 = 
"
";
//N = Succ<Succ<Zero>>, M = Zero, Sum = Succ<Succ<Succ<Zero>>>
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
