# copybook-rs Context

## Project Overview
`copybook-rs` is a Rust toolkit designed for parsing COBOL copybooks and converting mainframe data (EBCDIC/ASCII) to JSON and back with deterministic round-trip fidelity. It prioritizes correctness, safety (zero `unsafe` in public APIs), and performance.

**Status:** Engineering Preview (v0.4.2-dev)

## Workspace Structure
The project is a Cargo workspace consisting of the following crates:

*   **`copybook-core`**: Core parsing logic, AST, and schema types for COBOL copybooks.
*   **`copybook-codec`**: Encoders/decoders for COBOL data types, character conversion, and record framing.
*   **`copybook-cli`**: The command-line interface (`copybook`) for parsing, inspecting, decoding, and encoding.
*   **`copybook-gen`**: Utilities for generating test fixtures and synthetic data.
*   **`copybook-bench`**: Performance benchmarks and testing harness.
*   **`copybook-arrow`**: Integration with Apache Arrow for data processing.
*   **`xtask`**: Internal task automation tool.
*   **`examples/kafka_pipeline`**: Example of a Kafka processing pipeline.

## Common Commands (Using `just`)
The project uses `just` as a task runner.

*   **Build:** `just build` (or `cargo build --workspace`)
*   **Test:** `just test` (uses `cargo-nextest`)
*   **Lint:** `just lint` (runs `clippy` with pedantic warnings)
*   **Format:** `just fmt` (runs `cargo fmt`)
*   **CI Check (Local):** `just ci-quick` (runs fmt, clippy, build, nextest)
*   **Benchmarks:** `just bench` or `just bench-json`
*   **Documentation:** `just docs`

## Development Conventions
*   **Rust Version:** MSRV 1.90+, Edition 2024.
*   **Safety:** **Zero `unsafe` code** is allowed in public APIs.
*   **Linting:** Strictly enforces `clippy::pedantic`.
*   **Testing:** High test coverage required. Uses `nextest` for speed.
    *   **Golden Fixtures:** Used for structural validation (`tests/golden_fixtures_comprehensive.rs`).
*   **Commits:** Follow [Conventional Commits](https://www.conventionalcommits.org/) (e.g., `feat(core): add ODO support`).
*   **Release:** Managed via `cargo-release` and `git-cliff`.

## Architecture Flow
1.  **Parse:** `copybook-core` lexes and parses COBOL copybooks into an AST.
2.  **Resolve:** Layouts are resolved (handling REDEFINES, ODO, etc.).
3.  **Codec:** `copybook-codec` uses the resolved layout to decode/encode binary data.
4.  **Interface:** `copybook-cli` exposes these capabilities to the user.

## Key Files
*   `Cargo.toml`: Workspace configuration.
*   `justfile`: Task definitions.
*   `README.md`: Project documentation and quick start.
*   `CONTRIBUTING.md`: Detailed contribution guide.
*   `docs/`: Extensive documentation (CLI reference, specs, roadmap).
