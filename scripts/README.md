<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scripts Directory

Automation and utility scripts for copybook-rs development and CI/CD.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **performance_test.rs** - Performance regression testing

## Development Automation
- **adapt-review-agents.py** - Compatibility wrapper for the Rust-native `copybook-scripts adapt-review-agents` command
- **final-cleanup-agents.py** - Compatibility wrapper for the Rust-native `copybook-scripts final-cleanup-agents` command
- **fix-agent-issues.py** - Compatibility wrapper for the Rust-native `copybook-scripts fix-agent-issues` command

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
scripts/adapt-review-agents.py

# Fix agent configuration issues
scripts/fix-agent-issues.py

# Run final cleanup
scripts/final-cleanup-agents.py
```

## Platform Support
- Shell scripts (.sh) for Unix-like systems
- Batch files (.bat) for Windows
- Rust-native maintenance logic in `tools/copybook-scripts` with small compatibility wrappers in `scripts/`

These scripts complement the main build system and are used for specialized development tasks.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
