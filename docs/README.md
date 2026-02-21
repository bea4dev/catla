# Catla Context Docs

This directory is the persistent handoff context for working on the Catla compiler/transpiler.

## Goal

Keep enough project state in-repo so a new Codex session (or a human) can continue work without relying on chat history.

## Read Order (Recommended)

1. `docs/handoff.md`
2. `docs/lifetime-analysis.md`
3. `src/catla_compiler/src/compiler.rs`
4. `src/catla_codegen/src/codegen.rs`

## What each file is for

- `docs/handoff.md`
  - Current status, architecture map, recent changes, known risks, next actions.
- `docs/lifetime-analysis.md`
  - Detailed explanation of the current constraint/lifetime-tree based allocation analysis.

## Verification commands

- `cargo check`
- `cargo check -p catla_codegen`
- `cargo test -p catla_compiler compiler -- --nocapture`
- `cargo test -p catla_compiler optimization_debug -- --nocapture`
- `cd .catla_compiler_test/test && cargo check`

## Generated directories

These are expected to be generated during compile/test runs and are already ignored:

- `.catla`
- `.catla_compiler_test`
- `.catla_optimization_debug`

## Handoff workflow

When finishing a major task:

1. Update `docs/handoff.md`:
   - What changed
   - Why
   - How verified
   - Remaining work
2. If lifetime logic changed, also update `docs/lifetime-analysis.md`.
3. Keep file paths and function names explicit so search is easy.
