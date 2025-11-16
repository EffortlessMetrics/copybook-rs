# Pull Request

## Summary
Brief description of the changes in this PR.

## Type of Change
- [ ] Bug fix (non-breaking change that fixes an issue)
- [ ] New feature (non-breaking change that adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to change)
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Refactoring (no functional changes)

## COBOL/Mainframe Context
- **COBOL features affected**: [e.g., OCCURS, REDEFINES, COMP-3, Level-88]
- **Data processing impact**: [e.g., parsing, encoding, decoding, validation]
- **Mainframe compatibility**: [e.g., IBM z/OS, specific COBOL compiler behavior]

## Changes Made
Detailed description of changes:

### Core Changes
- [ ] copybook-core: [describe parsing/schema changes]
- [ ] copybook-codec: [describe encoding/decoding changes]
- [ ] copybook-cli: [describe CLI interface changes]
- [ ] copybook-gen: [describe test generation changes]
- [ ] copybook-bench: [describe benchmark changes]

### Documentation
- [ ] Updated API documentation
- [ ] Updated CLI help text
- [ ] Updated examples
- [ ] Updated CHANGELOG.md

## Testing
- [ ] Added/updated unit tests
- [ ] Added/updated integration tests
- [ ] Added/updated golden fixtures
- [ ] Manual testing performed
- [ ] Performance testing (if applicable)

### Test Coverage
```bash
# Commands used for testing
cargo test --workspace
cargo nextest run --workspace
PERF=1 cargo bench --package copybook-bench
```

## Performance Impact
- [ ] No performance impact
- [ ] Performance improvement (provide benchmarks)
- [ ] Performance regression acceptable (provide justification)
- [ ] Performance regression needs investigation

### Benchmark Results (if applicable)
```
Before: [benchmark results]
After:  [benchmark results]
Change: [improvement/regression percentage]
```

## Infrastructure/Tooling Changes (if applicable)
**Complete this section if your PR modifies:**
- Support matrix registry or CLI
- Performance receipt parsing or SLO evaluation
- CI scripts or validation gates
- xtask automation or docs verification tools

### Semantic Validation
- [ ] `./scripts/ci/offline-semantic.sh` passes
- [ ] Adversarial tests completed (see checklist below)
- [ ] Manual verification of key calculations (if applicable)
- [ ] Documentation updated with validation evidence

### Adversarial Testing Evidence
For infrastructure changes, you must prove **correctness** — not just "does it compile?"

#### Drift Detection (support matrix changes)
- [ ] Temporarily modified/removed registry entry or docs row
- [ ] Verified tool detects drift with clear error message
- [ ] Restored and verified tool passes

#### Malformed Input Handling (parser/validation changes)
- [ ] Fed invalid JSON/data to new parsing logic
- [ ] Verified clear, actionable error messages
- [ ] Confirmed no silent failures or defaults

#### Boundary Cases (SLO/threshold changes)
- [ ] Tested exact boundary values (e.g., 80.0 pass, 79.9 fail)
- [ ] Verified correct behavior at limits
- [ ] Confirmed no off-by-one or floating-point errors

#### Manual Math Verification (perf/throughput changes)
- [ ] Recomputed key values by hand (bytes→MiB/s, percentages, deltas)
- [ ] Compared manual calculations to tool output
- [ ] Documented verification in PR description

#### Round-Trip Equality (serialization changes)
- [ ] Verified registry → JSON → parse produces identical data
- [ ] Tested with all supported formats/modes
- [ ] Confirmed no data loss or transformation

### Local Validation Commands Run
```bash
# Record the commands you ran and their results
./scripts/ci/offline-semantic.sh           # ✅ Pass / ❌ Fail (with justification)
cargo run -p xtask -- docs verify-support-matrix  # ✅ / ❌
cargo test -p xtask                        # ✅ / ❌
cargo test -p copybook-cli --test support_cli     # ✅ / ❌
```

### Evidence Summary
```markdown
<!-- Paste your validation evidence here. Example: -->

Local validation:
- ✅ `./scripts/ci/offline-semantic.sh` passed
- ✅ Drift detection: commented out level-66-renames row, tool failed with "Support matrix drift detected"
- ✅ Boundary validation: verified 80.0 MiB/s → Pass, 79.9 → Fail
- ✅ Manual math: recomputed MiB/s from bytes/ns in Python REPL, matches tool output
- ✅ Round-trip: registry → JSON → serde_json::from_str produces identical HashSet
```

## Breaking Changes
- [ ] No breaking changes
- [ ] Breaking changes with migration path (describe below)

### Migration Guide (if breaking)
Describe how users should update their code:
```rust
// Before
old_api_usage();

// After
new_api_usage();
```

## Checklist
- [ ] Code follows the project's style guidelines
- [ ] Self-review completed
- [ ] Code is well-commented, especially for complex COBOL parsing logic
- [ ] Documentation updated for any new features
- [ ] Tests added/updated and passing
- [ ] No new clippy warnings introduced
- [ ] CHANGELOG.md updated (if user-facing changes)

## Enterprise Validation
For enterprise-critical changes:
- [ ] Validated against enterprise performance targets
- [ ] Tested with real-world mainframe data (sanitized)
- [ ] Audit logging requirements considered
- [ ] Backward compatibility verified

## Related Issues
Closes #[issue_number]
Related to #[issue_number]

## Additional Notes
Any additional information that reviewers should know about this PR.