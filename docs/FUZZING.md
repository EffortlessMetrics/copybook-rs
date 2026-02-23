<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Fuzzing Guide for copybook-rs

This document explains the fuzzing infrastructure for the copybook-rs project, including how to run fuzzers locally, manage corpora, and triage issues.

## Overview

The copybook-rs project uses [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz) with [libFuzzer](https://llvm.org/docs/LibFuzzer.html) to perform continuous fuzz testing of critical code paths. Fuzzing helps discover edge cases, security vulnerabilities, and bugs that traditional testing might miss.

### Fuzzing Framework Choice

We chose **cargo-fuzz** with **libFuzzer** because:

1. **Rust Native**: cargo-fuzz is the standard fuzzing framework for Rust, with excellent integration
2. **Coverage-Guided**: libFuzzer uses coverage feedback to intelligently explore code paths
3. **Fast**: In-process fuzzing with minimal overhead
4. **Corpus Management**: Built-in corpus minimization and deduplication
5. **CI Integration**: Easy to integrate with GitHub Actions

## Fuzz Targets

The project includes the following fuzz targets:

| Target | Purpose | Coverage |
|--------|---------|----------|
| `copybook_parse` | Fuzz copybook syntax parsing | `copybook-core::parser` |
| `binary_decode` | Fuzz binary data decoding | `copybook-codec::decode` |
| `json_encode` | Fuzz JSON data encoding | `copybook-codec::encode` |
| `pic_clause` | Fuzz PIC clause parsing | `copybook-core::pic` |
| `occurs_odo` | Fuzz OCCURS/ODO handling | `copybook-core::layout` |
| `redefines` | Fuzz REDEFINES processing | `copybook-core::layout` |
| `record_io_dispatch` | Fuzz fixed-vs-RDW record dispatch facade | `copybook-record-io` |

### Target Descriptions

#### copybook_parse

Tests the copybook parser with various inputs including:
- Valid copybook syntax
- Malformed copybook syntax
- Edge cases (empty input, very long lines, nested structures)
- Unicode and special characters
- Various COBOL dialect features

#### binary_decode

Tests the decoder with various binary inputs including:
- Valid binary data matching copybook schema
- Malformed binary data
- Random bytes
- Edge cases (all zeros, all ones, patterns)
- Data with RDW headers
- Various codepage encodings

#### json_encode

Tests the JSON encoder with various inputs including:
- Valid JSON objects
- Malformed JSON
- Extreme values (very large numbers, deep nesting)
- Unicode strings
- Various data types (null, arrays, objects)

#### pic_clause

Tests PIC clause parsing with various inputs including:
- Valid PIC clauses (X, 9, S9, V, etc.)
- Invalid PIC clauses
- Edge cases (very long clauses, complex patterns)
- Mixed type clauses
- Edited PIC patterns

#### occurs_odo

Tests OCCURS and ODO clauses with various inputs including:
- Valid OCCURS clauses
- ODO clauses with valid dependencies
- Invalid ODO positioning
- Nested OCCURS
- Edge cases (zero bounds, very large bounds)

#### redefines

Tests REDEFINES clauses with various inputs including:
- Valid REDEFINES clauses
- Multiple REDEFINES on same field
- REDEFINES with OCCURS
- REDEFINES with ODO
- Invalid REDEFINES positioning
- Complex REDEFINES hierarchies

#### record_io_dispatch

Tests the record I/O dispatch microcrate with various inputs including:
- Fixed and RDW format selector permutations
- Valid and invalid fixed-format LRECL contracts
- RDW header/payload framing through dispatch helpers
- Raw malformed buffers and truncated inputs

## Running Fuzzers Locally

### Prerequisites

1. Install Rust (1.92.0 or later)
2. Install cargo-fuzz:
   ```bash
   cargo install cargo-fuzz --version 0.13.4
   ```

### Basic Usage

Run a fuzzer with default settings:

```bash
cd fuzz
cargo fuzz run copybook_parse
```

Run a specific fuzzer for a limited time:

```bash
cargo fuzz run binary_decode -- -max_total_time=60
```

Run a fuzzer with a specific number of iterations:

```bash
cargo fuzz run json_encode -- -runs=10000
```

### Fuzzer Options

Common libFuzzer options:

| Option | Description | Example |
|--------|-------------|---------|
| `-max_total_time=N` | Run for N seconds | `-max_total_time=300` |
| `-runs=N` | Run N iterations | `-runs=10000` |
| `-seed=N` | Set random seed | `-seed=12345` |
| `-jobs=N` | Run N parallel jobs | `-jobs=4` |
| `-workers=N` | Use N worker threads | `-workers=2` |
| `-timeout=N` | Timeout per input (ms) | `-timeout=1000` |

### Reproducing Crashes

To reproduce a crash found during fuzzing:

```bash
cargo fuzz run <target> <crash_file>
```

For example:

```bash
cargo fuzz run copybook_parse fuzz/artifacts/copybook_parse/crash-abc123
```

## Corpus Management

### Initial Corpus

The initial corpus is located in `fuzz/corpus/<target>/` and contains:

- Valid copybook examples from `fixtures/copybooks/`
- PIC clause examples from test cases
- Binary data samples from `test-data/`
- JSON examples from test fixtures

### Adding to Corpus

When you find interesting inputs (either from fuzzing or manual testing), add them to the corpus:

```bash
cp interesting_input.txt fuzz/corpus/copybook_parse/
```

### Corpus Minimization

To minimize corpus size while maintaining coverage:

```bash
cargo fuzz cmin <target>
```

This removes redundant inputs from the corpus.

### Corpus Deduplication

To remove duplicate inputs:

```bash
cargo fuzz cmin <target>
```

The `cmin` command performs both minimization and deduplication.

### Corpus Expansion

To expand corpus with new inputs while preserving existing coverage:

```bash
cargo fuzz tmin <target> <input_file>
```

This minimizes a specific input while preserving its unique coverage.

## CI Integration

Fuzzing runs automatically in CI:

1. **Nightly**: Runs at 2 AM UTC every day
2. **Manual**: Can be triggered via GitHub Actions UI
3. **PR**: Runs when fuzz-related files are changed

### Workflow: `.github/workflows/fuzz-integration.yml`

The CI workflow:

1. Installs cargo-fuzz
2. Runs each fuzz target for 5 minutes (configurable)
3. Captures crash artifacts if found
4. Minimizes corpus on main branch
5. Uploads artifacts for review

### Viewing Results

Fuzzing results are available in:

- GitHub Actions logs
- Uploaded artifacts (crash files, minimized corpus)
- GitHub Actions summary (fuzz report)

## Triage Process

### When a Crash is Found

1. **Download the crash artifact** from GitHub Actions
2. **Reproduce locally**:
   ```bash
   cargo fuzz run <target> <crash_file>
   ```
3. **Minimize the crash input**:
   ```bash
   cargo fuzz tmin <target> <crash_file>
   ```
4. **Add to regression tests**:
   - Create a test case in the appropriate test file
   - Add the minimized input as a fixture
5. **Create a bug report**:
   - Include crash file
   - Include backtrace
   - Include minimal reproduction case
6. **Fix the bug** and verify with fuzzer

### Bug Report Template

```markdown
## Fuzzer Crash Report

**Fuzzer Target:** `<target>`

**Crash File:** Attached

**Backtrace:**
```
<backtrace from fuzzer>
```

**Minimal Reproduction:**
```rust
// Test case that reproduces the crash
```

**Expected Behavior:**
<description of expected behavior>

**Actual Behavior:**
<description of actual behavior>
```

## Adding New Fuzz Targets

### Step 1: Create the Fuzzer

Create a new file in `fuzz/fuzz_targets/<new_target>.rs`:

```rust
#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Your fuzzing logic here
});
```

### Step 2: Register the Target

Add to `fuzz/Cargo.toml`:

```toml
[[bin]]
name = "new_target"
path = "fuzz_targets/new_target.rs"
```

### Step 3: Create Initial Corpus

Create `fuzz/corpus/new_target/` and add initial inputs:

```bash
mkdir -p fuzz/corpus/new_target
cp test_inputs/* fuzz/corpus/new_target/
```

### Step 4: Update CI

Add to `.github/workflows/fuzz-integration.yml` matrix:

```yaml
- target: new_target
  package: copybook-fuzz
```

### Step 5: Update Documentation

Add to this document's "Fuzz Targets" section.

## Best Practices

### Writing Good Fuzzers

1. **Keep it simple**: Don't add complex logic in the fuzzer
2. **Use existing APIs**: Call the actual code paths you want to test
3. **Handle errors gracefully**: Don't panic on expected errors
4. **Test multiple configurations**: Vary options to increase coverage
5. **Use deterministic seeds**: For reproducible results

### Corpus Management

1. **Regular minimization**: Run `cargo fuzz cmin` periodically
2. **Remove duplicates**: Keep corpus lean and focused
3. **Add edge cases**: Include interesting inputs from real-world usage
4. **Review growth**: Monitor corpus size and investigate sudden growth

### CI Configuration

1. **Balance coverage and time**: 5 minutes per target is a good default
2. **Parallel execution**: Run targets in parallel for faster results
3. **Artifact retention**: Keep crash artifacts for at least 90 days
4. **Scheduled runs**: Nightly runs catch regressions early

## Troubleshooting

### Common Issues

#### Fuzzer won't build

Ensure you have the correct Rust version and cargo-fuzz installed:

```bash
rustc --version  # Should be 1.92.0 or later
cargo install cargo-fuzz --version 0.13.4
```

#### No coverage increase

This is normal! Fuzzers may not find new coverage if:
- The code is already well-covered
- The input format is constrained
- The fuzzer needs more time

Try:
- Running for longer
- Adding more diverse corpus inputs
- Checking if the fuzzer is actually exercising the target code

#### Too many crashes

If you're getting many crashes:
1. Check if they're all the same issue
2. Minimize the crash inputs
3. Add assertions to catch the issue earlier
4. Consider if the fuzzer is testing too much at once

## Performance Considerations

### Fuzzer Speed

- Aim for >1000 exec/s per fuzzer
- Reduce work in the fuzzer harness
- Use simple test cases in corpus

### Corpus Size

- Keep corpus under 10MB per target
- Regular minimization helps
- Remove redundant inputs

### CI Duration

- Total fuzzing time: ~30-45 minutes
- Can be reduced by:
  - Running fewer targets
  - Reducing time per target
  - Running only on schedule

## Additional Resources

- [cargo-fuzz documentation](https://github.com/rust-fuzz/cargo-fuzz)
- [libFuzzer documentation](https://llvm.org/docs/LibFuzzer.html)
- [Rust Fuzz Book](https://rust-fuzz.github.io/book/)
- [Fuzzing Introspection](https://github.com/google/fuzzing/blob/master/docs/introspector.md)

## Contributing

When contributing fuzzing improvements:

1. Test locally before pushing
2. Document any new targets
3. Update this guide as needed
4. Add corpus inputs for new features
5. Review crash artifacts regularly

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
