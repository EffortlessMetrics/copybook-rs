# Validation Protocol — Quick Reference

## When to Use Each Script

| Script | When to Use | What It Validates | Time |
|--------|------------|-------------------|------|
| `./scripts/ci/offline-semantic.sh` | **WSL/unstable environments** (default) | Format, drift detection, semantic tests (+ perf summary if available) | ~1-2 min |
| `./scripts/ci/quick.sh` | Stable systems, opportunistic hygiene | Above + clippy pedantic, broader tests | ~3-5 min |
| `./scripts/ci/offline-all.sh` | Pre-merge, stable systems only | Full CI including benchmarks | ~8-15 min |

**Golden Rule**: If full rebuilds trigger rustc ICEs, use `offline-semantic.sh` as your hard gate.

---

## Infrastructure PR Checklist

For PRs touching **support matrix**, **perf receipts**, **CI**, or **xtask**:

### 1. Run Semantic Validation
```bash
./scripts/ci/offline-semantic.sh
```
✅ **Must pass** before committing

### 2. Adversarial Tests

#### Support Matrix Changes
```bash
# 1. Break the docs intentionally
vim docs/reference/COBOL_SUPPORT_MATRIX.md  # comment out a row

# 2. Verify tool catches it
cargo run -p xtask -- docs verify-support-matrix
# Expected: "Support matrix drift detected ... <feature-id>"

# 3. Restore
git checkout docs/reference/COBOL_SUPPORT_MATRIX.md
cargo run -p xtask -- docs verify-support-matrix  # should pass
```

#### Perf Receipt Changes
```bash
# 1. Test malformed JSON
# Backup any existing perf.json
[ -f scripts/bench/perf.json ] && cp scripts/bench/perf.json scripts/bench/perf.json.bak

# Write deliberately invalid JSON
cat > scripts/bench/perf.json << 'EOF'
{"bad": "json",}
EOF

# Run perf summary – should fail with clear parse error
cargo run -p xtask -- perf --summarize-last || echo "✅ perf summary failed as expected on malformed JSON"

# Restore original if it existed
[ -f scripts/bench/perf.json.bak ] && mv scripts/bench/perf.json.bak scripts/bench/perf.json

# 2. Test boundary cases (80.0 pass, 79.9 fail)
# Backup current perf.json (if present)
[ -f scripts/bench/perf.json ] && cp scripts/bench/perf.json scripts/bench/perf.json.bak

# 80.0 MiB/s should PASS
cat > scripts/bench/perf.json << EOF
{
  "display_mibps": 80.0,
  "comp3_mibps": 40.0,
  "status": "test",
  "commit": "boundary-pass",
  "timestamp": "$(date -Iseconds)",
  "toolchain": "$(rustc --version | cut -d' ' -f2)"
}
EOF

cargo run -p xtask -- perf --summarize-last  # expect ✅ SLO pass

# 79.9 MiB/s should FAIL
cat > scripts/bench/perf.json << EOF
{
  "display_mibps": 79.9,
  "comp3_mibps": 40.0,
  "status": "test",
  "commit": "boundary-fail",
  "timestamp": "$(date -Iseconds)",
  "toolchain": "$(rustc --version | cut -d' ' -f2)"
}
EOF

cargo run -p xtask -- perf --summarize-last && echo "❌ expected failure but got success"

# Restore original perf.json
[ -f scripts/bench/perf.json.bak ] && mv scripts/bench/perf.json.bak scripts/bench/perf.json

# 3. Manual math verification
# Open Python REPL and verify:
# MiB/s = (bytes / (nanoseconds / 1e9)) / (1024**2)
# Compare to tool output
```

#### Support CLI Changes
```bash
# 1. Test JSON round-trip equality
cargo run --bin copybook -- support --json > /tmp/support.json

# 2. Verify in Rust test or manual check:
# HashSet from registry == HashSet from JSON

# 3. Test feature lookup
cargo run --bin copybook -- support --check level-88  # exit 0
cargo run --bin copybook -- support --check level-66-renames  # exit non-zero
cargo run --bin copybook -- support --check nonexistent  # exit non-zero with error
```

### 3. Record Evidence in PR

Paste into PR description:
```markdown
## Semantic Validation

Local validation:
- ✅ `./scripts/ci/offline-semantic.sh` passed
- ✅ Adversarial tests: [describe what you broke and how tool caught it]
- ✅ Boundary validation: [exact thresholds tested]
- ✅ Manual math: [calculations verified by hand]
- ✅ Round-trip: [registry ↔ CLI ↔ JSON equality confirmed]
```

---

## What "Semantically Validated" Means

✅ **It parses correctly** — not just compiles, but processes inputs as expected
✅ **It fails correctly** — adversarial tests show it catches errors
✅ **It's mathematically sound** — manual verification of calculations
✅ **It's drift-resistant** — registry ↔ docs ↔ CLI stay in sync

---

## Common Validation Commands

### Support Matrix
```bash
# Verify drift detection
cargo run -p xtask -- docs verify-support-matrix

# Test CLI output formats
cargo run --bin copybook -- support
cargo run --bin copybook -- support --json
cargo run --bin copybook -- support --format json --status supported

# Test feature lookup
cargo run --bin copybook -- support --check level-88
```

### Performance Receipts
```bash
# Parse and validate receipt
cargo run -p xtask -- perf --summarize-last

# Run perf unit tests
cargo test -p xtask perf

# Run perf integration tests
cargo test -p xtask --test perf_integration
```

### Full Workspace Tests
```bash
# All xtask tests (perf + docs)
cargo test -p xtask

# Support CLI tests
cargo test -p copybook-cli --test support_cli

# Support matrix unit tests
cargo test -p copybook-core support_matrix
```

---

## Manual Math Verification Examples

### Throughput Calculation
Given a performance receipt:
```json
{
  "display_heavy": {
    "total_bytes": 100000000,
    "elapsed_ns": 487804878
  }
}
```

Verify by hand:
```python
# Python REPL
bytes = 100_000_000
ns = 487_804_878
seconds = ns / 1e9  # 0.487804878
mib = 1024 ** 2     # 1048576
mibps = (bytes / seconds) / mib  # should match tool output

# Expected: ~205 MiB/s
```

### SLO Delta Calculation
```python
# From receipt
current = 205.4
slo = 80.0
delta_pct = ((current - slo) / slo) * 100
# Expected: +156.75%
```

Compare to tool output in summary.

---

## Troubleshooting

### "rustc ICE: WouldBlock" in WSL
- **Cause**: Resource exhaustion during full rebuilds
- **Solution**: Use `offline-semantic.sh` instead of `offline-all.sh`
- **Validation**: Semantic guarantees still proven via targeted tests

### "Support matrix drift detected" (expected failure)
- **Good**: This means the tool is working
- **Action**: Restore the docs/registry and verify it passes

### Test failures in `xtask`
- **Check**: Did you run `cargo fmt` first?
- **Check**: Are you using Rust 1.90+ (MSRV)?
- **Debug**: Run individual tests: `cargo test -p xtask perf::tests::test_name -- --nocapture`

---

## Environment Notes

### WSL2 Limitations
- Rustc ICEs during full workspace rebuilds
- Mitigation: Use semantic validation scripts
- All semantic guarantees still provable via unit/integration tests

### CI Instability
- GitHub Actions may be temporarily unreliable
- Local validation via `offline-semantic.sh` is sufficient for merge readiness
- Full CI validation will happen when Actions stabilizes

---

## Example Workflow

```bash
# 1. Make infrastructure changes
vim copybook-core/src/support_matrix.rs

# 2. Run semantic validation
./scripts/ci/offline-semantic.sh

# 3. Adversarial test
vim docs/reference/COBOL_SUPPORT_MATRIX.md  # break it
cargo run -p xtask -- docs verify-support-matrix  # should fail
git checkout docs/reference/COBOL_SUPPORT_MATRIX.md  # restore
cargo run -p xtask -- docs verify-support-matrix  # should pass

# 4. Manual verification (if perf changes)
python3 <<EOF
bytes, ns = 100_000_000, 487_804_878
print(f"MiB/s: {(bytes / (ns / 1e9)) / (1024**2):.1f}")
EOF

# 5. Commit with evidence
git add -u
git commit -m "feat(infra): add support matrix validation"

# 6. Push and create PR with validation evidence
git push origin feature/your-branch
```

---

## Questions?

- **CONTRIBUTING.md**: Full semantic validation guide
- **PR Template**: Infrastructure-specific checklist
- **Issues**: Create a GitHub issue for validation questions
