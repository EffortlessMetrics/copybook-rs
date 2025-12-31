# Phase 1 Baseline Documentation

**Project**: copybook-rs v0.4.2-dev  
**Branch**: main  
**CI Mode**: CI-off with local gates and small PRs

This document captures manual gate commands and local validation procedures during CI-off mode. It serves as a "proof over claims" reference by documenting known-good operator commands.

---

## Local Gate Receipt Template

Every PR body should include a completed Local Gate Receipt. Copy this template and fill in the details.

```markdown
# Local Gate Receipt

## Format: v0.4.2-dev

**Date**: YYYY-MM-DD
**PR**: #XXX (link to PR)

## Local Gates (ALL PRs MUST pass)

```bash
# Format check
cargo fmt --all --check

# Lint check
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Build check (release mode for CLI/codec)
cargo build --workspace --release

# Test check
cargo test --workspace -j 2

# Exit code mapping verification
cargo test -p copybook-cli --tests exit_code_mapping
```

## Status
✅ All gates passed

**Performance** (only if touched):
```bash
# Run benchmarks and validate receipts
bash scripts/bench.sh
bash scripts/validate-perf-receipt.sh scripts/bench/perf.json
```

## Notes
- Any deviations from expected results should be documented
```

---

## Manual Validation Commands (when CI returns)

When CI is offline, run these commands to validate workflows and ensure artifact upload paths are correct.

### Mutants Baseline

```bash
# Run mutants baseline for core package
cargo mutants --package copybook-core --baseline

# Run mutants baseline for codec package
cargo mutants --package copybook-codec --baseline

# Run mutants baseline for CLI package
cargo mutants --package copybook-cli --baseline
```

**Expected Output**: JSON baseline file created at `target/criterion/mutants/canonical_baseline.json`

**Purpose**: Establishes a baseline for mutation testing to detect code quality regressions.

---

### Fuzz Baseline

```bash
# Run fuzz baseline for core package
cargo fuzz --package copybook-core --baseline

# Run fuzz baseline for codec package
cargo fuzz --package copybook-codec --baseline

# Run fuzz baseline for CLI package
cargo fuzz --package copybook-cli --baseline
```

**Expected Output**: JSON baseline file created at `target/fuzz/canonical_baseline.json`

**Purpose**: Establishes a baseline for fuzz testing to detect edge cases and potential vulnerabilities.

---

### SBOM Local Command

```bash
# Generate Software Bill of Materials (SBOM)
cargo cyclonedx --manifest-path copybook-cli/Cargo.toml --format json
mv copybook-cli/copybook-cli.cdx.json sbom.cdx.json
```

**Expected Output**: SBOM file at `sbom-cyclonedx/sbom.cdx.json`

**Purpose**: Documents all dependencies and their versions for security and compliance tracking.

---

### Determinism Local Command

```bash
# Run determinism validation smoke test
cargo run -p xtask -- determinism-smoke
```

**Expected Output**: JSON receipts with hash comparisons showing deterministic build behavior

**Purpose**: Validates that builds produce consistent, reproducible outputs.

---

## Expected Outputs Summary

| Command | Expected Output Location | Purpose |
|---------|-------------------------|---------|
| `cargo mutants --baseline` | `target/criterion/mutants/canonical_baseline.json` | Mutation testing baseline |
| `cargo fuzz --baseline` | `target/fuzz/canonical_baseline.json` | Fuzz testing baseline |
| `cargo cyclonedx` | `sbom-cyclonedx/sbom.cdx.json` | Software Bill of Materials |
| `cargo run -p xtask -- determinism-smoke` | JSON receipts with hash comparisons | Build determinism validation |

---

## Phase 1 Focus Areas

- **Quality Gates**: Regression prevention through manual validation
- **Baseline Establishment**: Capturing known-good states for comparison
- **Documentation**: "Proof over claims" approach with executable commands

---

## Phase 2 Blockers

The following items are blocking Phase 2 progress:

1. **Edited PIC E3 encode**: Implementation pending
2. **Dialect Lever (#51)**: Issue tracking dialect support

---

## Notes

- All commands should be run from the project root directory
- Baseline files should be committed when establishing new baselines
- Any deviations from expected outputs should be investigated and documented
- This documentation will be updated as Phase 1 evolves into Phase 2
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
