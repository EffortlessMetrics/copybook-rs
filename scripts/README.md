<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scripts Directory

Automation and utility scripts for copybook-rs development and CI/CD.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **soak-aggregate.sh** - Thin compatibility wrapper for the native `copybook-scripts soak-aggregate` Rust implementation
- **performance_test.rs** - Performance regression testing

## Repository Checks
- **check-public-result-docs.sh** - Rust-backed public `Result` API documentation and attribute guard
- **check_no_unwrap_expect.sh** - Rust-backed panic-call guard
- **check_no_new_test_panic.sh** - diff-based guard against new `panic!` macros
- **guard-hotpaths.sh** - Rust-backed hot-path allocation guard

## Development Automation
- **setup-dev.sh** - One-shot bootstrap that installs the cargo subcommands the justfile expects (cargo-nextest, cargo-deny, cargo-watch, cargo-llvm-cov, cargo-mutants); also runnable as `just setup`
- **copybook-scripts adapt-review-agents** - Native Rust agent configuration adaptation utility
- **copybook-scripts final-cleanup-agents** - Native Rust agent cleanup and finalization
- **copybook-scripts fix-agent-issues** - Native Rust agent configuration repair tool
- **adapt-review-agents.py**, **final-cleanup-agents.py**, and **fix-agent-issues.py** - Compatibility wrappers that delegate to the Rust tool

## Usage

Scripts are typically run as part of development workflows:

### Performance Testing
```bash
# Run benchmarks (Unix)
./scripts/bench.sh

# Run benchmarks (Windows)
.\scripts\bench.bat
```

### Agent Management
```bash
# Adapt agent configurations
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- adapt-review-agents

# Fix agent configuration issues
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- fix-agent-issues

# Compatibility wrappers remain available for existing automation
python scripts/adapt-review-agents.py
python scripts/fix-agent-issues.py
```

## Platform Support
- Shell scripts (.sh) for Unix-like systems; new logic should prefer native Rust commands in `tools/copybook-scripts` with shell wrappers only for compatibility
- Batch files (.bat) for Windows
- Native Rust utilities in `tools/copybook-scripts` for repository automation
- Python scripts (.py) are compatibility wrappers for existing automation

These scripts complement the main build system and are used for specialized development tasks. Shell entrypoints are thin compatibility wrappers where a Rust implementation exists under `tools/copybook-scripts`.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
