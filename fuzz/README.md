<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Fuzzing Infrastructure

This directory contains the fuzzing infrastructure for the copybook-rs project using [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz) with [libFuzzer](https://llvm.org/docs/LibFuzzer.html).

## Quick Start

### Install Dependencies

```bash
cargo install cargo-fuzz --version 0.13.4
```

### Run a Fuzzer

```bash
# Run the copybook parser fuzzer
cargo fuzz run copybook_parse

# Run for a specific duration
cargo fuzz run binary_decode -- -max_total_time=60

# Run with a specific number of iterations
cargo fuzz run json_encode -- -runs=10000
```

### Reproduce a Crash

```bash
cargo fuzz run <target> <crash_file>
```

## Fuzz Targets

| Target | Description |
|--------|-------------|
| `copybook_parse` | Fuzz copybook syntax parsing |
| `binary_decode` | Fuzz binary data decoding |
| `json_encode` | Fuzz JSON data encoding |
| `pic_clause` | Fuzz PIC clause parsing |
| `occurs_odo` | Fuzz OCCURS/ODO handling |
| `redefines` | Fuzz REDEFINES processing |
| `rdw_header` | Fuzz RDW header parsing/framing primitives |
| `fixed_record` | Fuzz fixed-length (LRECL) record framing primitives |
| `determinism_compare` | Fuzz determinism hash/diff comparison primitives |

## Directory Structure

```
fuzz/
├── Cargo.toml              # Fuzzing package configuration
├── README.md               # This file
├── fuzz_targets/           # Fuzz target implementations
│   ├── copybook_parse.rs
│   ├── binary_decode.rs
│   ├── json_encode.rs
│   ├── pic_clause.rs
│   ├── occurs_odo.rs
│   ├── redefines.rs
│   ├── rdw_header.rs
│   ├── fixed_record.rs
│   └── determinism_compare.rs
└── corpus/                 # Initial corpus for each target
    ├── copybook_parse/
    ├── binary_decode/
    ├── json_encode/
    ├── pic_clause/
    ├── occurs_odo/
    ├── redefines/
    ├── rdw_header/
    ├── fixed_record/
    └── determinism_compare/
```

## Corpus Management

### Add New Inputs to Corpus

```bash
cp interesting_input.txt corpus/copybook_parse/
```

### Minimize Corpus

```bash
cargo fuzz cmin copybook_parse
```

### Minimize a Specific Input

```bash
cargo fuzz tmin copybook_parse interesting_input.txt
```

## CI Integration

Fuzzing runs automatically in CI via `.github/workflows/fuzz-integration.yml`:

- **Nightly**: Runs at 2 AM UTC every day
- **Manual**: Can be triggered via GitHub Actions UI
- **PR**: Runs when fuzz-related files are changed

## Documentation

For detailed documentation, see [`docs/FUZZING.md`](../docs/FUZZING.md).

## Troubleshooting

### Build Issues

Ensure you have the correct Rust version:

```bash
rustc --version  # Should be 1.92.0 or later
cargo install cargo-fuzz --version 0.13.4
```

### No Coverage Increase

This is normal! Fuzzers may not find new coverage if:
- The code is already well-covered
- The input format is constrained
- The fuzzer needs more time

## Contributing

When adding new fuzz targets:

1. Create the fuzzer in `fuzz_targets/`
2. Add to `Cargo.toml`
3. Create initial corpus in `corpus/`
4. Update CI workflow
5. Update documentation

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
