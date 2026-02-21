# Lifetime Analysis Design (Current Implementation)

This document explains the current allocation/lifetime optimization in `src/catla_optimization/src/lifetime.rs`.

## High-level objective

For class objects (`new Class { ... }`), choose between:

- stack allocation (`AllocationKind::Stack`)
- heap allocation (`AllocationKind::Heap`)

while preserving lifetime constraints inferred from:

- field ownership relationships (`a.field = b` style)
- function call constraints across modules
- static/global roots (`static` variable reachability)

The analysis also computes per-call argument borrow hints (`call_argument_borrow`) and variable origin mapping (`variable_origins`) for debug/inspection.

## Inputs and outputs

## Inputs

Per module (`ModuleLifetimeSource`):

- AST (`CatlaAST`)
- type infer result map (`type_infer_results`)
- module entity type map (`module_entity_type_map`)
- name resolution map (`name_resolved_map`)

Global:

- `GlobalUserTypeSet`
- merged `ImplementsInfoSet` (for interface dispatch equivalence)

## Output (`LifetimeAnalyzeResults`)

- `object_results: HashMap<EntityID, LifetimeAnalyzeResult>`
  - final stack/heap decision + `requires_drop`
- `call_argument_borrow: HashMap<EntityID, bool>`
  - call argument expressions that are safe candidates for `.borrow()`
- `variable_origins: HashMap<EntityID, HashSet<EntityID>>`
  - which allocation origins flow into each variable

Entry points:

- multi-module: `evaluate_lifetime_sources(...)`
- single-program helper: `analyze_lifetime(...)`

## Core model: flow + lifetime tree + constraints

### 1) ExprFlow

`ExprFlow` tracks, for each expression:

- `params`: parameter indexes it depends on
- `origins`: concrete `new object` origin entities
- `tree_refs`: lifetime-tree nodes reachable from that value

### 2) Lifetime tree (`LifetimeTreeArena`)

Each class allocation origin gets a tree node (`LifetimeTreeRef`), with:

- `children`: field edges (`owner -> owner.field`)
- `borrow_refs`: borrow relationships
- `alloc_origin`: backing allocation entity
- `drop_order`: source-order heuristic (span start)
- `has_static_lifetime`: static root reachability flag

`all_expected_constraints()` auto-generates constraints for all child/borrow edges.

### 3) Constraint sets in `FunctionState`

- `parameter_constraints: (shorter_param, longer_param)`
- `tree_constraints: (shorter_tree, longer_tree)`
- `param_to_tree_constraints: (shorter_param, longer_tree)`

Interpretation: lifetime of `shorter` must be no longer than `longer`.

## Algorithm phases

## Phase A: function discovery and dispatch graph

`collect_function_definitions(...)` gathers function symbols and parameter bindings.

`build_interface_adjacency(...)` links interface function symbols and implementation function symbols so summary merging can conservatively handle dynamic dispatch.

## Phase B: summary fixed-point

For each function, `analyze_function_summary(...)` computes:

- `parameter_escape`: whether parameter i escapes
- `parameter_constraints`: i < j constraints among parameters

This is repeated to fixed-point (`while changed`) because summaries depend on call targets, and call targets include interface-equivalent implementations.

## Phase C: final per-function analysis

`analyze_function_with_final_summary(...)` re-runs body analysis using converged summaries and records concrete object/tree constraints.

Then `solve_constraints_for_function(...)` promotes allocations to heap until constraints are satisfied (or stable).

## Phase D: module-level root analysis

`analyze_module_level_roots(...)` analyzes module-level statements (including static roots) and also calls `solve_constraints_for_function(...)`.

This is where `static` reachability becomes a heap-promoting root.

## Statement/expression behavior details

## Assignment

In `analyze_program_statements(...)` for `Statement::Assignment`:

- Analyze RHS flow first.
- If LHS is a plain variable binding, alias maps (`aliases_to_*`) are updated.
- If assignment crosses function boundary, RHS may escape.
- Otherwise add constraint between LHS flow and RHS flow.

## Variable define with `static`

For static variables:

- expression tree refs are marked static (`mark_static_lifetime`)
- escape is applied to expression flow

This intentionally pushes reachable origins toward heap.

## Function calls

For calls in `analyze_primary(...)` and `analyze_primary_left(...)`:

1. Resolve candidate callee symbols.
2. Merge `parameter_escape` / `parameter_constraints` from all candidates + interface-equivalent symbols.
3. Apply escape on call inputs where required.
4. Add constraints from merged parameter constraints.
5. If output mode enabled, mark borrowable call args in `call_argument_borrow`.

Unknown call targets are treated conservatively (escape defaults true through merged rules).

## New object

For class `new` expressions:

- create origin node + owner flow
- add field child nodes
- connect borrow/constraint edges from field flows
- initialize object result as stack candidate, then solver may promote to heap

## Constraint solving and promotion

`solve_constraints_for_function(...)` loop:

1. Start from explicit `tree_constraints` + `all_expected_constraints()`.
2. Promote `param_to_tree_constraints` longer side to heap (conservative).
3. Check each tree constraint with `is_tree_constraint_satisfied(...)`.
4. If violated, promote longer tree origin to heap.
5. Repeat until no changes (guarded loop).

`is_tree_constraint_satisfied(...)` logic (important):

- If longer is already heap -> satisfied.
- If longer has static lifetime -> satisfied.
- If shorter is heap but longer is not -> violated.
- Otherwise compare lifetime ordering via `is_tree_shorter(...)`:
  - static beats non-static
  - else drop order (`shorter_order >= longer_order`)

Heap promotion is monotonic (`Stack -> Heap` only), enabling convergence.

## Borrow hint (`call_argument_borrow`)

The analysis marks an argument as borrowable only when:

- the argument expression is a simple variable literal expression
- variable type is class
- merged callee summary says that argument does not escape

This is used for debug output and can be consumed by codegen when desired.

## Relationship to codegen

Current codegen usage of lifetime output:

- stack-vs-heap for class `new` object allocation
- stack slot materialization (`CatlaObjectRef::stack(...)` path)
- `requires_drop` propagation

Current function-argument `.borrow()` insertion in codegen is type-based (callee parameter type resolution), not directly from `call_argument_borrow`.

## Important caveats

- `CatlaObjectRef::stack(...)` uses `transmute` to `'static`; analysis mistakes can produce UAF.
- Heap promotion is conservative, but unsoundness risk exists if alias/escape extraction misses a path.
- `is_argument_tree` field exists but is not yet used by solver logic.
- Many parser/type/codegen branches remain `todo!()`; unsupported syntax can bypass optimization assumptions.

## Practical debugging

Use:

- `cargo test -p catla_compiler optimization_debug -- --nocapture`

Ariadne output is printed from `catla_print_debug` with byte->char conversion (`byte_range_to_char_range`) so UTF-8 spans are displayed correctly.

Label examples:

- object allocation: stack/heap
- variable lifetime aggregate: stack/heap/unknown
- borrow argument markers
