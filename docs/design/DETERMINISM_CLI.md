# Determinism CLI Design

**Status**: Implemented (Issue #112 Phase 2, PR #160)
**Author**: System
**Date**: 2025-11-18

> Phase 1 (codec harness) shipped in PR #158. Phase 2 (CLI wiring) shipped in PR #160.
> Phase 3 (CI smoke tests) is tracked in Issue #112 as a follow-up.

## Overview

Provides operator-facing CLI commands for determinism validation of copybook encode/decode operations. Wraps the low-level `copybook_codec::determinism` module with ergonomic subcommands and structured output.

## Goals

1. **Explicit verification**: Operators can prove determinism without modifying existing workflows
2. **Structured output**: Both human-readable and JSON formats for CI integration
3. **Clear semantics**: Distinct exit codes for deterministic, non-deterministic, and error cases
4. **Bounded execution**: At-most-two passes over data (no unbounded repetition)

## Non-Goals

- **Automatic fixing**: Determinism validation observes; it does not mutate data
- **Statistical analysis**: No multiple-run averaging or confidence intervals
- **Performance tuning**: Not a benchmarking tool (use `cargo bench`)

---

## CLI Surface

### Subcommand Family

Explicit `copybook determinism` subcommand with three modes:

```bash
copybook determinism <MODE> [OPTIONS] <COPYBOOK> <INPUT>
```

**Modes:**
- `decode` - Verify identical JSON output across two decode operations
- `encode` - Verify identical binary output across two encode operations
- `round-trip` - Verify decode→encode→decode consistency

### Common Options

```
OPTIONS:
    --format <FORMAT>           Record format (fixed, rdw) [default: fixed]
    --codepage <CODEPAGE>       EBCDIC codepage (cp037, cp1047, etc.) [default: cp037]
    --json-number <MODE>        JSON number handling (lossless, f64) [default: lossless]
    --emit-meta                 Include metadata in JSON output
    --output <FORMAT>           Output format (human, json) [default: human]
    --max-diffs <N>             Maximum byte differences to report [default: 100]
    -h, --help                  Print help
```

---

## Command Examples

### 1. Decode Determinism

Verify that decoding the same binary data produces identical JSON twice.

```bash
copybook determinism decode \
  --format fixed \
  --codepage cp037 \
  schema.cpy data.bin
```

**What it does:**
1. Parse `schema.cpy` → `Schema`
2. Call `check_decode_determinism(&schema, &data, &options)`
3. Compare BLAKE3 hashes of two JSON outputs
4. Exit 0 if deterministic, 1 if not

**Example human output:**

```
Determinism Check: DECODE
Schema: schema.cpy
Input: data.bin (1024 bytes)
Codepage: CP037
Format: Fixed-length

✅ DETERMINISTIC

Round 1 Hash: blake3:7a3f9e2b...
Round 2 Hash: blake3:7a3f9e2b...
Byte Differences: 0
```

**Example JSON output (`--output json`):**

```json
{
  "mode": "decode",
  "schema_path": "schema.cpy",
  "input_path": "data.bin",
  "input_size_bytes": 1024,
  "is_deterministic": true,
  "round1_hash": "blake3:7a3f9e2b...",
  "round2_hash": "blake3:7a3f9e2b...",
  "byte_differences": null
}
```

### 2. Encode Determinism

Verify that encoding the same JSON produces identical binary twice.

```bash
copybook determinism encode \
  --format fixed \
  --codepage cp037 \
  schema.cpy input.jsonl
```

**What it does:**
1. Parse `schema.cpy` → `Schema`
2. Parse first line of `input.jsonl` → JSON
3. Call `check_encode_determinism(&schema, &json, &options)`
4. Compare BLAKE3 hashes of two binary outputs

**Example output (non-deterministic case):**

```
Determinism Check: ENCODE
Schema: schema.cpy
Input: input.jsonl (1 record)
Codepage: CP037
Format: Fixed-length

❌ NON-DETERMINISTIC

Round 1 Hash: blake3:7a3f9e2b...
Round 2 Hash: blake3:1b8c4d5e...
Byte Differences: 3

Offset  Round1  Round2
------  ------  ------
0x0010  0x40    0x41
0x0011  0x40    0x42
0x0012  0x40    0x43
```

### 3. Round-Trip Determinism

Verify decode→encode→decode produces identical final JSON.

```bash
copybook determinism round-trip \
  --format fixed \
  --codepage cp037 \
  schema.cpy data.bin
```

**What it does:**
1. Parse `schema.cpy` → `Schema`
2. Call `check_round_trip_determinism(&schema, &data, &decode_opts, &encode_opts)`
3. Compare BLAKE3 hashes of original JSON vs re-decoded JSON

**Example output:**

```
Determinism Check: ROUND-TRIP
Schema: schema.cpy
Input: data.bin (1024 bytes)
Codepage: CP037
Format: Fixed-length

✅ DETERMINISTIC

Original JSON Hash: blake3:7a3f9e2b...
Re-decoded Hash:    blake3:7a3f9e2b...
Byte Differences: 0
```

---

## Exit Codes

The CLI uses the existing `ExitCode` enum mapping. Semantics:

| Code | Meaning | ExitCode Variant | When |
|------|---------|------------------|------|
| `0` | **Deterministic** | `ExitCode::Ok` | Hashes match, no drift detected |
| `2` | **Non-deterministic** | `ExitCode::Data` | Hashes differ, byte diffs reported |
| `3` | **Codec/usage error** | `ExitCode::Encode` | Decode/encode/options failure |

**Implementation Notes:**
- Deterministic result → `ExitCode::Ok` (process exit 0)
- Non-deterministic result → `ExitCode::Data` (validation failure, exit 2)
- Codec/usage errors → `ExitCode::Encode` or `ExitCode::Internal` (exit 3 or 5)

**CI Integration:**
Only "deterministic vs non-deterministic" matters for pipelines. Any non-zero exit code should be treated as "fail" in automation scripts.

**CI Usage:**

```bash
# Fail CI if non-deterministic
copybook determinism decode schema.cpy data.bin || exit 1

# Allow non-determinism but capture result
copybook determinism decode schema.cpy data.bin --output json > result.json
RESULT=$?
if [ $RESULT -eq 1 ]; then
  echo "Warning: Non-deterministic decode detected"
fi
```

---

## Output Formats

### Human-Readable (Default)

**Layout:**
1. Header: Mode, schema, input, options
2. Verdict: ✅ DETERMINISTIC or ❌ NON-DETERMINISTIC
3. Hashes: BLAKE3 hex strings (first 16 chars)
4. Diffs: Byte-level table (if any, max 100 by default)

**Design principles:**
- Clear visual hierarchy (headers, symbols)
- Truncated hashes (full hashes in JSON mode)
- Diff table only if non-deterministic

### JSON (Structured)

**Schema:**

```json
{
  "mode": "decode" | "encode" | "round-trip",
  "schema_path": "string",
  "input_path": "string",
  "input_size_bytes": number,
  "is_deterministic": boolean,
  "round1_hash": "blake3:hex",
  "round2_hash": "blake3:hex",
  "byte_differences": [
    {
      "offset": number,
      "round1_byte": number,
      "round2_byte": number
    }
  ] | null
}
```

**Features:**
- Machine-parseable for CI/CD pipelines
- Full BLAKE3 hashes (64 hex chars)
- Complete diff arrays (limited by `--max-diffs`)

---

## Implementation Constraints

### 1. Two-Pass Limit

Each validation is **at-most-two passes**:
- `decode`: Decode twice
- `encode`: Encode twice
- `round-trip`: Decode once, encode once, decode once (3 operations but 1 pass over input)

**No unbounded repetition** - this is determinism verification, not statistical analysis.

### 2. Memory Efficiency

- Streaming input where possible (reuse file handle)
- Diff reporting capped at `--max-diffs` (default 100)
- No in-memory duplication of large data structures

### 3. Error Propagation

Codec errors (CBKD*, CBKE*) return **exit code 3** with structured error message:

```json
{
  "error": "codec_error",
  "error_code": "CBKD101",
  "message": "Truncated COMP-3 field at offset 0x0042",
  "schema_path": "schema.cpy",
  "input_path": "data.bin"
}
```

### 4. No Data Mutation

Determinism validation is **read-only**:
- Never writes to input files
- Never modifies binary/JSON data
- Purely observational

---

## Testing Strategy

### Unit Tests

Test adapter logic (CLI → determinism module):
- Argument parsing
- Exit code mapping
- Output formatting (human vs JSON)
- Error message rendering

### Integration Tests

Small end-to-end tests with tiny fixtures:
- `test_decode_determinism_happy_path`
- `test_encode_nondeterminism_detected`
- `test_round_trip_with_json_output`
- `test_codec_error_exit_code_3`

### Golden Fixtures

Reuse existing golden fixtures for validation:
- Pick 2-3 representative fixtures (DISPLAY, COMP-3, ODO)
- Run `copybook determinism decode` as smoke test in CI

---

## Next Steps

1. **PR B Implementation**
   - Add `copybook-cli/src/determinism.rs` (CLI adapter)
   - Wire into `copybook-cli/src/main.rs` subcommand routing
   - Add unit tests for argument parsing and exit codes
   - Add 3 integration tests (decode/encode/round-trip)

2. **Documentation Updates**
   - Update `CLAUDE.md` with new commands
   - Update `README.md` with determinism validation section
   - Add examples to `docs/REPORT.md` quality assurance features

3. **CI Integration (PR C)**
   - Add `.github/workflows/determinism.yml`
   - Run 1-2 determinism checks on golden fixtures
   - Advisory-only initially (non-blocking)

---

## Related Issues

- **Issue #112**: Determinism validation (Phase 1: core, Phase 2: CLI, Phase 3: CI)
- **PR #158**: Core determinism module implementation
- **PR B**: CLI wiring (this design doc)
- **PR C**: CI smoke tests
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
