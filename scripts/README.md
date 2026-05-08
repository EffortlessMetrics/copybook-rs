<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scripts Directory

Automation and utility scripts for copybook-rs development and CI/CD.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **performance_test.rs** - Performance regression testing

## Development Automation
- **copybook-scripts adapt-review-agents** - Agent configuration adaptation utility
- **copybook-scripts final-cleanup-agents** - Agent cleanup and finalization
- **copybook-scripts fix-agent-issues** - Agent configuration repair tool

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

# Run final cleanup after bulk edits
cargo run --quiet --manifest-path tools/copybook-scripts/Cargo.toml -- final-cleanup-agents
```

## Platform Support
- Shell scripts (.sh) for Unix-like systems
- Batch files (.bat) for Windows
- Rust automation via `tools/copybook-scripts` for repository maintenance tasks

These scripts complement the main build system and are used for specialized development tasks.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
