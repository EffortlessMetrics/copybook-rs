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