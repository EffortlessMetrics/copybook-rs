<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scripts Directory

Automation and utility scripts for copybook-rs development and CI/CD.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **performance_test.rs** - Performance regression testing

## Repository Checks
- **check-public-result-docs.sh** - Rust-backed public `Result` API documentation and attribute guard
- **check_no_unwrap_expect.sh** - Rust-backed panic-call guard
- **guard-hotpaths.sh** - Rust-backed hot-path allocation guard

## Development Automation
- **adapt-review-agents.py** - Agent configuration adaptation utility
- **final-cleanup-agents.py** - Agent cleanup and finalization
- **fix-agent-issues.py** - Agent configuration repair tool

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
python scripts/adapt-review-agents.py

# Fix agent configuration issues
python scripts/fix-agent-issues.py
```

## Platform Support
- Shell scripts (.sh) for Unix-like systems
- Batch files (.bat) for Windows
- Python scripts (.py) for cross-platform automation

These scripts complement the main build system and are used for specialized development tasks. Shell entrypoints are thin compatibility wrappers where a Rust implementation exists under `tools/copybook-scripts`.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
