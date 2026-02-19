<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Copilot Instructions for copybook-rs

## Project Overview
copybook-rs is a Rust workspace for enterprise mainframe data processing (COBOL copybooks to JSON/Binary).
**Key Goals**: Correctness, Safety (0 unsafe code), and Performance (streaming I/O, scratch buffers).
**Status**: Engineering Preview (v0.4.0).

## Build, Test, and Lint
This project uses `just` for task management.

- **Run all tests**: `just test` (uses `cargo-nextest`) or `cargo test --workspace`
- **Run a single test**: `cargo nextest run --workspace -E 'test(test_name)'`
- **Run benchmarks**: `just bench` (outputs to `scripts/bench/perf.json`)
- **Lint**: `just lint` (enforces `clippy::pedantic`)
- **Format**: `just fmt`
- **Build Release**: `just build-release`
- **Full CI Check**: `just ci-quick` (runs fmt, clippy, build, nextest)

## High-Level Architecture
1.  **copybook-core**: Parses COBOL copybook text -> Schema AST. Handles Layout resolution.
2.  **copybook-codec**: Schema -> Encode/Decode (Binary <-> JSON). Handles data conversion.
3.  **copybook-cli**: User interface. Orchestrates core and codec.
4.  **copybook-bench**: Performance benchmarks and regression detection.

## Key Conventions

### Error Handling
Use the structured error taxonomy with stable codes:
- `CBKP*`: Parse errors (syntax, unsupported features)
- `CBKS*`: Schema validation (ODO counters, limits)
- `CBKD*`: Data errors (invalid decimals, truncation)
- `CBKE*`: Encoding errors (type mismatches)
- `CBKR*`: Record format errors (RDW)

### Performance Patterns
- **ScratchBuffers**: Use `copybook_codec::memory::ScratchBuffers` for hot loops to avoid allocation.
- **Streaming**: Prefer `iter_records_from_file` over loading entire files.
- **Zero-Copy**: Use patterns that minimize copying where possible.

### Testing
- **Golden Fixtures**: use `copybook_gen::golden::GoldenTest` for structural validation (ODO, Level-88).
- **Determinism**: Tests must verify deterministic output for both Encode and Decode.
- **Benchmarks**: Performance is tracked. Run `just bench` to generate receipts.

### Code Style
- **No Unsafe**: Do not introduce `unsafe` blocks in public APIs.
- **Clippy Pedantic**: Code must pass `clippy::pedantic`.
- **MSRV**: Rust 1.92+.

### Feature Specifics
- **Dialects**: Respect `Dialect` enum (Normative, ZeroTolerant, OneTolerant) for ODO `min_count` handling.
- **Field Projection**: Use `project_schema` for `--select` functionality (handles ODO dependencies automatically).
- **Edited PIC**: Supported in 3 phases: Parse (E1), Decode (E2), Encode (E3).
