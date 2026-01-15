# Pull Request

## Summary

<!-- Provide a brief description of what this PR does and why -->

## Type of Change

<!-- Mark the relevant option with an 'x' -->

- [ ] Feature (new functionality)
- [ ] Bugfix (fixes an issue)
- [ ] Refactor (code improvement without behavior change)
- [ ] Performance (optimization)
- [ ] Documentation (README, docs/, code comments)
- [ ] Tests (test additions or improvements)
- [ ] CI/CD (workflow or tooling changes)
- [ ] Chore (dependencies, formatting, etc.)

## COBOL/Mainframe Context

<!-- If applicable, describe COBOL features or mainframe compatibility considerations -->

- **COBOL features affected**: [e.g., OCCURS, REDEFINES, COMP-3, Level-88, edited PIC, ODO]
- **Data processing impact**: [e.g., parsing, encoding, decoding, validation]
- **Mainframe compatibility**: [e.g., IBM Enterprise COBOL, dialect considerations]

## Workspace Crates Affected

<!-- Mark crates modified in this PR -->

- [ ] copybook-core (parsing, schema, AST)
- [ ] copybook-codec (encoding, decoding, character conversion)
- [ ] copybook-cli (CLI commands and options)
- [ ] copybook-gen (test fixture generation)
- [ ] copybook-bench (performance benchmarks)

## Checklist

<!-- Mark completed items with an 'x' -->

- [ ] Tests added/updated (or N/A for docs-only changes)
- [ ] Documentation updated (CLAUDE.md, docs/, or inline comments if needed)
- [ ] CHANGELOG.md updated (if user-facing change)
- [ ] `cargo fmt --all` passes
- [ ] `cargo clippy --workspace -- -D warnings -W clippy::pedantic` passes
- [ ] `cargo test --workspace` passes (or `cargo nextest run --workspace`)
- [ ] Follows [conventional commit format](https://www.conventionalcommits.org/) (e.g., `feat(core):`, `fix(codec):`)
- [ ] MSRV compliance (Rust 1.90+) verified if dependencies changed
- [ ] Golden fixtures updated if applicable

## Testing Instructions

<!-- Describe how to test this PR. Include commands, test cases, or manual steps -->

```bash
# Example testing commands
cargo test --workspace
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# If performance-related
PERF=1 cargo bench --package copybook-bench
cargo run --bin bench-report -p copybook-bench -- compare scripts/bench/perf.json

# If CLI-related
cargo run --bin copybook -- [command] [args]
```

## Performance Impact

<!-- If applicable, provide benchmark results or performance analysis -->

- [ ] No performance impact expected
- [ ] Performance improvement (provide evidence below)
- [ ] Performance regression acceptable (provide justification)
- [ ] Performance tested with baseline comparison

### Benchmark Results (if applicable)

```
Before: [benchmark results or baseline reference]
After:  [benchmark results]
Change: [improvement/regression percentage]
```

## Infrastructure/Tooling Changes (if applicable)

<!-- Complete this section if your PR modifies CI/CD, xtask, or support matrix tooling -->

**Areas modified:**
- [ ] Support matrix registry or CLI
- [ ] Performance receipt parsing or SLO evaluation
- [ ] CI scripts or validation gates
- [ ] xtask automation or docs verification tools

### Validation Evidence (for infrastructure changes)

```bash
# Record validation commands and results
./scripts/ci/offline-semantic.sh           # ✅ Pass / ❌ Fail
cargo run -p xtask -- docs verify-support-matrix  # ✅ / ❌
cargo test -p xtask                        # ✅ / ❌
```

**Adversarial Testing** (if applicable):
- [ ] Tested with malformed input (verified error handling)
- [ ] Verified boundary cases (e.g., threshold values)
- [ ] Manual calculation verification for numeric outputs
- [ ] Drift detection validated (for support matrix changes)

## Breaking Changes

- [ ] No breaking changes
- [ ] Breaking changes with migration path (describe below)

### Migration Guide (if breaking)

```rust
// Before
old_api_usage();

// After
new_api_usage();
```

## Enterprise Validation (if applicable)

For enterprise-critical changes:
- [ ] Validated against enterprise performance targets (DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s)
- [ ] Tested with real-world mainframe data (sanitized)
- [ ] Error taxonomy codes documented (CBKP*/CBKS*/CBKD*/CBKE*/CBKR*)
- [ ] Backward compatibility verified

## Related Issues

<!-- Link related issues using 'Closes #xxx', 'Fixes #xxx', or 'Relates to #xxx' -->

Closes #

## Additional Notes

<!-- Optional: Add any additional context, design decisions, trade-offs, or screenshots -->