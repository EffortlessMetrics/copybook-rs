<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Tools Directory

Development-only crates for copybook-rs (`publish = false`).

## Crates

### copybook-bench

Performance benchmarks with regression detection and baseline management.

```bash
cargo bench -p copybook-bench
cargo run --bin bench-report -p copybook-bench -- baseline show
cargo run --bin bench-report -p copybook-bench -- compare scripts/bench/perf.json
```

### copybook-gen

Test fixture generation with golden fixture framework for structural validation.

```bash
cargo test -p copybook-gen
cargo run --package copybook-gen -- generate-golden-fixtures --enterprise --output fixtures/enterprise/
```

### copybook-scripts

Small Rust-native replacements for repository-maintenance shell scripts.

```bash
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- check-no-unwrap-expect
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- guard-hotpaths
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- perf-annotate-host
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- soak-dispatch
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- clean-merge-conflicts path/to/file.rs
```

### xtask

Build automation tasks for workspace-level operations.

```bash
cargo xtask <task>
```

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
