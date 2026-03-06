<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #102: RDW Codec Test Regression - Field Naming Inconsistency

## Context

The RDW (Record Descriptor Word) codec implementation has a field naming inconsistency that causes 6 comprehensive test failures in the copybook-codec test suite. The root cause is in `copybook-codec/src/lib_api.rs:241`, where the `decode_record_with_raw_data_and_scratch` function uses `_raw` as the field name instead of the standard `__raw_b64` used consistently throughout the codebase.

**Affected Components:**
- **copybook-codec**: Data encoding/decoding with RDW format support
- **Test Coverage**: 6 comprehensive RDW tests failing

**Data Processing Pipeline Impact:**
- **Data Encoding/Decoding**: RDW format raw data capture broken
- **CLI Processing**: `decode` command with `--emit-raw` flag may produce inconsistent field names
- **Output**: JSON output field naming violates established conventions

**Enterprise Impact:**
- **HIGH**: Blocks variable-length record processing workflows
- **Data Quality**: Inconsistent field naming breaks downstream JSON consumers
- **Roundtrip Fidelity**: Raw data preservation broken for RDW format
- **Test Coverage**: 6 comprehensive tests failing, blocking CI/CD validation

**Failing Tests:**
1. `test_rdw_raw_preservation_normative` - RDW raw data capture with RecordRDW mode
2. `test_rdw_length_recomputation` - RDW length recomputation on payload modification
3. `test_rdw_error_context` - Error context propagation in lenient mode
4. `test_rdw_with_odo_variable_length` - RDW with ODO variable arrays
5. `test_rdw_raw_preservation_with_reserved` - RDW raw preservation with non-zero reserved bytes
6. `test_rdw_raw_record_only_mode` - RDW Record mode (excluding RDW header)

## User Story

As a **mainframe data engineer**, I want **RDW codec operations to use consistent field naming (`__raw_b64`)** so that **JSON output is predictable, downstream consumers can reliably access raw data, and roundtrip encoding/decoding preserves data fidelity**.

## Acceptance Criteria

AC1: Fix field naming in `copybook-codec/src/lib_api.rs:241` from `_raw` to `__raw_b64` in `decode_record_with_raw_data_and_scratch` function for RawMode::Record and RawMode::RecordRDW modes

AC2: Verify `test_rdw_raw_preservation_normative` passes with correct `__raw_b64` field name when using RawMode::RecordRDW

AC3: Verify `test_rdw_raw_preservation_with_reserved` passes with correct `__raw_b64` field name preservation

AC4: Verify `test_rdw_raw_record_only_mode` passes with correct `__raw_b64` field name for RawMode::Record (excluding RDW header)

AC5: Fix `test_rdw_length_recomputation` by ensuring raw data is captured with correct field name during decode, enabling successful roundtrip encoding with recomputed RDW length

AC6: Fix `test_rdw_error_context` to properly propagate error context (record_index, byte_offset) in lenient mode for incomplete RDW headers

AC7: Fix `test_rdw_with_odo_variable_length` by ensuring ODO counter values are properly decoded in RDW format (address root cause beyond field naming if present)

AC8: Run full RDW test suite with `cargo test --package copybook-codec --features comprehensive-tests rdw` and verify all 37 RDW-related tests pass (21 unit tests + 16 comprehensive tests)

AC9: Verify no performance regression in RDW decoding via `cargo bench --package copybook-bench -- rdw` (if RDW benchmarks exist)

AC10: Update documentation in `CLAUDE.md` or relevant RDW codec documentation to clarify `__raw_b64` field naming convention for all RawMode variants

AC11: Verify field naming consistency across all RawMode variants (Off, Record, RecordRDW, Field) in `decode_record_with_raw_data_and_scratch`

AC12: Run comprehensive workspace validation with `cargo test --workspace --features comprehensive-tests` to ensure no regressions in other codec functionality

AC13: Verify roundtrip fidelity with `cargo test --package copybook-codec --test binary_roundtrip_fidelity_tests -- rdw` passes

AC14: Ensure zero unsafe code maintained (`cargo clippy --workspace -- -D warnings -W clippy::pedantic` passes)

AC15: Verify proper error taxonomy for RDW errors (CBKR* error codes) remains intact after fixes

## Technical Implementation Notes

**Affected Crates:**
- **copybook-codec**: Primary fix location in `src/lib_api.rs`

**Pipeline Stages:**
- **Data Encoding/Decoding**: RDW raw data capture field naming
- **CLI Processing**: `--emit-raw` flag handling for RDW format
- **Output**: JSON field naming consistency

**Performance Considerations:**
- No expected performance impact (field name string constant change)
- Verify no regression via benchmark suite if RDW benchmarks exist
- Memory efficiency unchanged (string literal substitution)
- Zero unsafe code enforcement maintained

**RDW Parsing Requirements:**
- Field naming must match `__raw_b64` convention established in `src/json.rs`, `src/odo_redefines.rs`
- RawMode::RecordRDW must capture full record including 4-byte RDW header
- RawMode::Record must capture payload only (excluding RDW header)
- Reserved bytes (bytes 2-3 of RDW) must be preserved in raw data capture

**Enterprise Validation:**
- RDW format is critical for variable-length mainframe records
- Field naming consistency essential for JSON schema stability
- Roundtrip encoding/decoding fidelity required for data integrity
- Error context must include record_index and byte_offset for troubleshooting

**Workspace Features:**
- Fix must work with `comprehensive-tests` feature flag
- No impact on standard test suite (non-comprehensive)
- Maintain compatibility with all codepage variants

**RDW Compatibility:**
- Big-endian length field (bytes 0-1) parsing unchanged
- Reserved bytes handling (strict vs lenient mode) unchanged
- Zero-length record handling unchanged
- ASCII corruption heuristic unchanged

**Testing Strategy:**
- **TDD Approach**: Map each AC to specific test assertions with `// AC:ID` tags
- **Comprehensive Validation**: Run all 37 RDW tests (unit + comprehensive)
- **Roundtrip Testing**: Validate encode(decode(data)) == data with raw preservation
- **Error Validation**: Verify error context propagation in failure scenarios
- **Regression Testing**: Full workspace test suite to catch unexpected impacts

**Root Cause Analysis:**
- **Primary Issue**: Field naming inconsistency in `lib_api.rs:241` (`_raw` vs `__raw_b64`)
- **Secondary Issues**:
  - Error context not propagated in lenient mode for incomplete RDW headers
  - Possible ODO interaction with RDW format (test_rdw_with_odo_variable_length)
  - Length recomputation may fail due to missing raw data field

**Implementation Plan:**
1. Fix field naming in `decode_record_with_raw_data_and_scratch` (AC1)
2. Verify raw preservation tests pass (AC2-AC4)
3. Address error context propagation (AC6)
4. Fix length recomputation roundtrip (AC5)
5. Investigate ODO+RDW interaction if still failing (AC7)
6. Run full validation suite (AC8, AC12-AC15)
7. Update documentation (AC10)

**Error Code Coverage:**
- `CBKR211_RDW_RESERVED_NONZERO`: Reserved bytes non-zero warning
- `CBKR311_RDW_UNDERFLOW`: Incomplete RDW header or payload
- `CBKE501_JSON_TYPE_MISMATCH`: Invalid base64 in `__raw_b64` field
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement (if AC7 reveals ODO issue)

**Zero Unsafe Code:**
- No unsafe code introduced
- All changes in safe Rust string literal substitution

**Deterministic Processing:**
- Field naming change does not affect determinism
- RDW parsing order unchanged
- Thread safety maintained
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
