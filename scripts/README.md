<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scripts Directory

Automation and utility wrappers for copybook-rs development and CI/CD.

Repository-maintenance logic that can run natively in Rust lives in the
`tools/copybook-scripts` crate. The shell files in this directory are thin,
backward-compatible launchers that bootstrap Cargo and dispatch to that crate.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **performance_test.rs** - Performance regression testing

## Development Automation
- **adapt-review-agents.sh** - Rust-backed agent configuration adaptation utility
- **fix-agent-issues.sh** - Rust-backed agent configuration repair tool
- **final-cleanup-agents.sh** - Rust-backed agent cleanup and finalization

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
./scripts/adapt-review-agents.sh

# Fix agent configuration issues
./scripts/fix-agent-issues.sh

# Final cleanup pass
./scripts/final-cleanup-agents.sh
```

The equivalent native commands can also be run directly:

```bash
cargo run --manifest-path tools/copybook-scripts/Cargo.toml -- adapt-review-agents
cargo run --manifest-path tools/copybook-scripts/Cargo.toml -- fix-agent-issues
cargo run --manifest-path tools/copybook-scripts/Cargo.toml -- final-cleanup-agents
```

## Platform Support
- Shell wrappers (.sh) for Unix-like systems
- Batch files (.bat) for Windows benchmark workflows
- Rust-backed utility commands in `tools/copybook-scripts`

These scripts complement the main build system and are used for specialized development tasks.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
