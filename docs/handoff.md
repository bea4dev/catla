# Handoff Notes (Current State)

Last updated: 2026-02-21

## Project summary

Catla is a language that transpiles to Rust.

Main flow:

1. parse
2. name resolve
3. type inference
4. lifetime optimization
5. Rust codegen
6. generated crate build/test

Key entry points:

- compile orchestration: `src/catla_compiler/src/compiler.rs`
- codegen library API: `src/catla_codegen/src/lib.rs`
- Rust emission logic: `src/catla_codegen/src/codegen.rs`
- lifetime optimization: `src/catla_optimization/src/lifetime.rs`
- optimization debug printing: `src/catla_print_debug/src/optimization.rs`
- runtime object model: `src/catla_std/src/object.rs`

## Recent implemented behavior (important)

## 1) `pub mod` tree generation for generated crates

`ensure_pub_mod_tree(...)` in compiler appends missing `pub mod ...;` declarations by scanning package module paths.

## 2) interface codegen

Catla interface maps to Rust trait emission.

## 3) Catla numeric type mapping

Builtin types like `int8/uint32/float64` map to Rust `i8/u32/f64` in codegen.

## 4) same-crate path handling

Same-crate root path is mapped to `crate` in generated Rust imports/type paths.

## 5) static variable codegen

Static variables are emitted with `LazyLock`:

- mutable static (`static var`) -> `LazyLock<RwLock<T>>`
- immutable static (`static let`) -> `LazyLock<T>`

Static class object initialization uses `CatlaObjectRef::heap(..., mutex=true, ...)`.

## 6) class allocation mode and stack slots

For class `new` expressions:

- allocation decision uses lifetime results
- stack allocations use generated stack slots (`MaybeUninit` + `CatlaObjectRef::stack`)

## 7) function argument borrow/clone insertion in codegen

Current behavior:

- function-call arguments: if callee parameter type is class -> emit `.borrow()`
- otherwise class value reads default to `.clone()`

Implementation detail:

- callee parameter types are resolved from type information (not ad-hoc pre-collected bool maps)
- resolution path in codegen:
  1. `type_infer_results` function type for literal
  2. fallback through `name_resolved_map` + `module_entity_type_map`

Relevant functions:

- `resolve_callee_function_type_for_literal(...)`
- `function_argument_usage(...)`

## 8) per-class object model generation (RC runtime shape change)

Codegen now emits a class-specific object carrier type per class (e.g. `__CatlaObject_Foo`) and class references are emitted as:

- `catla_std::object::CatlaObjectRef<__CatlaObject_Foo>`

`catla_std::object::CatlaObject` generic struct was removed from runtime and replaced with a trait-based object contract implemented by generated per-class object carriers.

Current runtime points:

- refcount operations live in `CatlaObjectRef` (`add_count`/`sub_count`)
- generated carriers provide count/value pointers via `catla_std::object::CatlaObject` trait
- `CatlaObjectRef` now implements `Deref<Target = T>`

Drop-codegen points:

- class-level drop hook impl target is generated carrier type (`impl catla_std::dispose::Drop for __CatlaObject_Foo`)
- explicit Catla `implements Drop for Foo { ... }` is currently consumed into generated carrier drop impl

## 9) `::` class path rewriting in expressions

Codegen now rewrites class symbols on `::` paths to generated carrier names.

Examples:

- `Foo::bar()` -> `__CatlaObject_Foo::bar()`
- `pkg::Foo::bar()` -> `pkg::__CatlaObject_Foo::bar()`

The class判定 uses resolved entity metadata (`name_resolved_map` + `module_entity_type_map`) so same-name types across modules are distinguished by entity, not just string match.

For compatibility, class member functions are now emitted to both:

- `impl Foo { ... }`
- `impl __CatlaObject_Foo { ... }`

## Lifetime analysis summary

See full detail in `docs/lifetime-analysis.md`.

Short version:

- Uses constraint-based analysis over flows + lifetime tree.
- Collects constraints from assignment, field flow, calls, and static roots.
- Solves by monotonic heap promotion until constraints are satisfied.
- Supports cross-module fixed-point by collecting all module sources and iterating function summaries.

## Codegen API contract (current)

`catla_codegen::codegen(...)` now receives:

- AST
- `type_infer_results`
- `name_resolved_map`
- `module_entity_type_map`
- `user_type_set`
- optional `lifetime_analyze_results`
- settings
- module path

If this signature changes, update both:

- `src/catla_codegen/src/lib.rs`
- call sites in `src/catla_compiler/src/compiler.rs`

## Validation commands (known good)

- `cargo check`
- `cargo check -p catla_codegen`
- `cargo test -p catla_compiler compiler -- --nocapture`
- `cargo test -p catla_compiler optimization_debug -- --nocapture`
- `cd .catla_compiler_test/test && cargo check`

## Debugging and diagnostics

Optimization debug output:

- `catla_compiler::test::optimization_debug`
- uses Ariadne labels
- span conversion is byte-range to char-range (important for UTF-8)

## Known risks / caveats

1. `CatlaObjectRef::stack(...)` relies on `transmute` to `'static`; analysis errors can be unsound.
2. Several branches in parser/type/codegen are still `todo!()`.
3. Borrow insertion in codegen is currently type-based, not directly using `LifetimeAnalyzeResults::call_argument_borrow`.
4. `is_argument_tree` exists in optimizer state but is not currently used in constraint solving behavior.

## Deferred design notes (2026-02-21, per-class object model refactor)

The following are intentionally deferred while moving from shared `CatlaObject<T>` to per-class generated object layout:

1. **`drop` body restrictions**
   - Planned direction: fully disallow `this.clone()` (or any operation that resurrects self) during `drop`.
   - Error reporting/enforcement is deferred; behavior may be unchecked temporarily.

2. **RC metadata access safety model**
   - A trait/mechanism to retrieve ref-count metadata from class values will be needed.
   - Sealing/soundness strategy is deferred.

3. **Language-level orphan rule policy**
   - Planned direction: prohibit Catla `implements` forms that would violate Rust coherence (external trait + external concrete).
   - Parser/type-checker enforcement is deferred.

4. **Code size / API ergonomics tradeoffs**
   - Per-class generated object wrapper and ref-count plumbing may increase generated code size.
   - `Deref`-based method resolution edge cases are acknowledged and deferred.
5. **UFCS-style explicit receiver calls**
   - Target support: `Struct::func(instance)` style calls for `&self` methods.
   - Current path rewrite may route this to `__CatlaObject_*::func(...)`, but full auto-borrow/autoref behavior is not yet finalized.

## Suggested next engineering tasks

1. Unify borrow insertion with `call_argument_borrow` output (optional policy gate).
2. Add regression tests for cross-module constraint propagation chains.
3. Add stress tests for static-root heap propagation (`a.field=b`, `b.field=c`, static assignment path).
4. Add negative tests for unsupported syntax paths that currently hit `todo!()`.
5. Consider documenting/encoding formal invariants for tree constraints and promotion rules.

## Session handoff template

Copy this section when handing off:

- Goal:
- Files edited:
- Behavior changed:
- Verification run:
- Known limitations:
- Next concrete step:
