<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scripts Directory

Automation and utility scripts for copybook-rs development and CI/CD.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **performance_test.rs** - Performance regression testing

## Development Automation
- **adapt-review-agents.sh** - Rust-backed agent configuration adaptation utility
- **final-cleanup-agents.sh** - Rust-backed agent cleanup and finalization
- **fix-agent-issues.sh** - Rust-backed agent configuration repair tool

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
```

## Platform Support
- Shell scripts (.sh) for Unix-like systems
- Batch files (.bat) for Windows
- Rust-native maintenance commands in `tools/copybook-scripts`, exposed through shell wrappers

These scripts complement the main build system and are used for specialized development tasks.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
