# Release Runbook

**Purpose**: One-command release process for copybook-rs crates.io publishing.

## Prerequisites

1. **Environment Setup**:
   ```bash
   # Ensure clean working directory
   git status

   # Ensure all tests pass
   cargo test --workspace --release

   # Ensure benchmarks work
   PERF=1 cargo bench --package copybook-bench
   ```

2. **Secrets Configuration**:
   - `CARGO_REGISTRY_TOKEN` must be set in GitHub repository secrets
   - Token must have permission to publish new crates on crates.io

## Release Process

### Step 1: Version Bump & Validation

```bash
# Current version is already 0.4.0 with pinned dependencies

# Validate all packages build correctly
cargo build --workspace --release

# Validate dry-run publishing works
cargo publish -p copybook-core --dry-run --allow-dirty
cargo publish -p copybook-codec --dry-run --allow-dirty
cargo publish -p copybook-cli --dry-run --allow-dirty

# Run full test suite
cargo test --workspace --release

# Validate benchmarks work and produce results
PERF=1 cargo bench --package copybook-bench
```

### Step 2: Commit & Tag

```bash
# Commit any final changes
git add .
git commit -m "chore(release): prepare v0.4.0 for crates.io publishing"

# Create and push release tag
git tag -a v0.4.0 -m "v0.4.0: Projection + edited PIC decode"
git push origin main --tags
```

### Step 3: Automated Publishing

The GitHub Actions workflow (`.github/workflows/publish.yml`) will automatically:

1. **Preflight Checks**:
   - Build workspace in release mode
   - Run full test suite
   - Validate all crates package correctly

2. **Publishing Sequence**:
   - Publish `copybook-core` (no dependencies)
   - Wait 90 seconds for crates.io index propagation
   - Publish `copybook-codec` (depends on core)
   - Wait 90 seconds for crates.io index propagation
   - Publish `copybook-cli` (depends on core + codec)

3. **Verification**:
   - Create GitHub Release with installation instructions
   - Run smoke test: `cargo add copybook-cli@0.4.0` in clean environment
   - Validate docs.rs build initiation

### Step 4: Post-Release Verification

**Manual Validation** (5-10 minutes after publishing):

```bash
# Test installation in clean environment
cargo new /tmp/copybook-smoke-test
cd /tmp/copybook-smoke-test

# Test cargo add works
cargo add copybook-cli@0.4.0

# Test build works
cargo build --quiet

# Test basic functionality
echo '01 TEST-RECORD.
   05 TEST-FIELD PIC X(10).' > test.cpy
echo 'Hello     ' | ./target/debug/copybook decode test.cpy /dev/stdin --format fixed
```

**Documentation Verification**:
- Visit https://docs.rs/copybook-core/0.4.0 (builds within 5-10 minutes)
- Visit https://docs.rs/copybook-codec/0.4.0
- Visit https://docs.rs/copybook-cli/0.4.0

**Crates.io Verification**:
- Visit https://crates.io/crates/copybook-core
- Visit https://crates.io/crates/copybook-codec
- Visit https://crates.io/crates/copybook-cli

## Troubleshooting

### Publishing Failures

**Index Propagation Issues**:
```bash
# If dependency resolution fails, wait longer and retry
sleep 180  # Wait 3 minutes instead of 90 seconds
cargo publish -p copybook-codec
```

**Authentication Issues**:
```bash
# Verify token has publish permissions
cargo login --registry crates-io
cargo publish -p copybook-core --dry-run
```

### Docs.rs Build Issues

**Build Timeouts**:
- Docs.rs has 30-minute build timeout
- Check docs.rs build logs for specific failures
- Ensure `include` lists don't contain large files

**Missing Dependencies**:
- Verify all dependencies are available on crates.io
- Check for dev-dependencies leaking into docs builds

### Smoke Test Failures

**Installation Issues**:
```bash
# Clear cargo cache if needed
cargo clean
rm -rf ~/.cargo/registry/cache
```

**Runtime Issues**:
```bash
# Check for missing system dependencies
ldd target/debug/copybook
```

## Rollback Procedure

**If Critical Issues Found**:

1. **Yank Bad Versions**:
   ```bash
   cargo yank --index crates-io copybook-cli@0.3.1
   cargo yank --index crates-io copybook-codec@0.3.1
   cargo yank --index crates-io copybook-core@0.3.1
   ```

2. **Fix Issues & Re-release**:
   ```bash
   # Fix the issues
   # Bump version to 0.3.2
   cargo set-version 0.3.2

   # Update workspace dependencies
   # Edit Cargo.toml: =0.3.1 → =0.3.2

   # Re-run release process
   git tag -a v0.3.2 -m "v0.3.2: Fix for yanked v0.3.1"
   ```

## Success Criteria

**Release is successful when**:
- ✅ All three crates published successfully to crates.io
- ✅ `cargo add copybook-cli@0.3.1` works in clean environment
- ✅ docs.rs pages build and are accessible
- ✅ GitHub Release created with correct metadata
- ✅ CI benchmarks run and report performance metrics
- ✅ All integration tests pass

**Production Ready Indicators**:
- ✅ Performance meets or exceeds SLOs (4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3)
- ✅ Memory usage stays under 256 MiB for multi-GB files
- ✅ 127+ tests passing with comprehensive coverage
- ✅ Golden fixtures enforce ODO structural constraints
- ✅ Error taxonomy provides stable diagnostic codes

## Next Steps After v0.3.1

1. **Monitor Adoption**:
   - Track download statistics on crates.io
   - Monitor GitHub issues for user feedback
   - Watch docs.rs build success rates

2. **v0.4.0 Planning**:
   - Implement remaining roadmap items
   - Add benchmark receipt CI integration
   - Enhance golden fixture coverage

3. **Ecosystem Integration**:
   - Create example integrations
   - Develop Arrow/Parquet adapters
   - Build enterprise connector prototypes

---

**Release Manager Checklist**:
- [ ] Prerequisites validated
- [ ] Version bumped and dependencies pinned
- [ ] Dry-run publishing successful
- [ ] Tests and benchmarks passing
- [ ] Tag created and pushed
- [ ] GitHub Actions workflow completed successfully
- [ ] Smoke tests passed
- [ ] Documentation verified
- [ ] Success criteria met
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
