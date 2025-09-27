# Scripts Directory

Automation and utility scripts for copybook-rs development and CI/CD.

## Performance Scripts
- **bench.sh** / **bench.bat** - Cross-platform benchmark execution
- **performance_test.rs** - Performance regression testing

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

These scripts complement the main build system and are used for specialized development tasks.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
