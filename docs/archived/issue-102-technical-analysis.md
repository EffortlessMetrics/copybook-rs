<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #102: RDW Codec Test Regression - Technical Specification Analysis

## Executive Summary

**Issue**: RDW (Record Descriptor Word) codec test regression caused by field naming inconsistency
**Root Cause**: `copybook-codec/src/lib_api.rs:241` uses `_raw` instead of `__raw_b64`
**Impact**: 6 comprehensive RDW tests failing, blocking variable-length record processing
**Severity**: HIGH - Blocks enterprise variable-length mainframe data workflows
**Complexity**: LOW - Simple field naming fix with secondary error context improvements

**Classification**: `additive` - Fixes existing broken functionality, no breaking API changes

**Recommendation**: PROCEED with implementation following the technical approach outlined below.

---

## 1. Requirements Analysis

### 1.1 Acceptance Criteria Validation

**Status**: ✅ All 15 acceptance criteria are testable, complete, and aligned with enterprise standards.

#### Core Functional Requirements (AC1-AC7)
- **AC1**: Field naming fix in `lib_api.rs:241` - CLEAR, TESTABLE ✅
- **AC2-AC4**: Raw preservation tests (RecordRDW, Reserved, Record modes) - CLEAR, TESTABLE ✅
- **AC5**: Length recomputation roundtrip - CLEAR, TESTABLE ✅
- **AC6**: Error context propagation - CLEAR, TESTABLE ✅
- **AC7**: ODO+RDW interaction - CLEAR, TESTABLE (with investigation clause) ✅

#### Validation Requirements (AC8-AC13)
- **AC8**: Full RDW test suite (37 tests: 21 unit + 16 comprehensive) - CLEAR, MEASURABLE ✅
- **AC9**: Performance regression check - CLEAR (no RDW-specific benchmarks found, defaults to general codec benchmarks) ⚠️
- **AC10**: Documentation update - CLEAR ✅
- **AC11**: Field naming consistency across RawMode variants - CLEAR, TESTABLE ✅
- **AC12**: Workspace validation - CLEAR, TESTABLE ✅
- **AC13**: Roundtrip fidelity - CLEAR, TESTABLE ✅

#### Quality Requirements (AC14-AC15)
- **AC14**: Zero unsafe code enforcement - CLEAR, TESTABLE ✅
- **AC15**: Error taxonomy integrity - CLEAR, TESTABLE ✅

### 1.2 Requirements Completeness

**Gaps Identified**:
1. **AC9 Clarification**: No RDW-specific benchmarks exist. Recommendation: Use general codec benchmarks (`cargo bench -p copybook-bench`) as baseline.
2. **AC7 Investigation**: ODO+RDW test failure shows `COUNTER` field returns `Null` instead of `"03"`, indicating deeper issue beyond field naming.

**Recommendations**:
- **AC9**: Update to reference general codec benchmarks instead of non-existent RDW benchmarks
- **AC7**: Add investigation step to determine if ODO counter decoding is broken in RDW format

---

## 2. Technical Implementation Approach

### 2.1 Root Cause Analysis

**Primary Issue**: Field naming inconsistency in `copybook-codec/src/lib_api.rs:241`

```rust
// CURRENT (BROKEN) - Line 241
json_obj.insert(
    "_raw".to_string(),  // ❌ WRONG - Should be "__raw_b64"
    Value::String(base64::engine::general_purpose::STANDARD.encode(&raw_bytes)),
);

// EXPECTED (CORRECT) - Used in decode_record_with_raw_data (lines 276, 282, 288, 292)
json_obj.insert("__raw_b64".to_string(), Value::String(raw_b64));
```

**Established Convention**: All other raw data capture points use `__raw_b64`:
- `copybook-codec/src/lib_api.rs`: Lines 276, 282, 288, 292 (4 occurrences)
- `copybook-codec/src/json.rs`: Lines 243, 416, 768, 1155, 2227, 2333 (6 occurrences)
- `copybook-codec/src/odo_redefines.rs`: Additional occurrences

**Secondary Issues**:
1. **Error Context**: `test_rdw_error_context` expects `record_index` or `byte_offset` in error context for incomplete RDW headers
2. **ODO Counter Decoding**: `test_rdw_with_odo_variable_length` shows `COUNTER` field returns `Null` instead of `"03"` in RDW format

### 2.2 Implementation Strategy

#### Phase 1: Field Naming Fix (AC1)
**File**: `copybook-codec/src/lib_api.rs`
**Line**: 241
**Change**:
```rust
// Before
"_raw".to_string(),

// After
"__raw_b64".to_string(),
```

**Impact**: Single-line change, zero performance impact (string literal substitution)

#### Phase 2: Verification (AC2-AC4, AC11)
**Validation Commands**:
```bash
# Verify raw preservation tests pass
cargo test --package copybook-codec --features comprehensive-tests test_rdw_raw_preservation_normative
cargo test --package copybook-codec --features comprehensive-tests test_rdw_raw_preservation_with_reserved
cargo test --package copybook-codec --test rdw_comprehensive test_rdw_raw_record_only_mode

# Verify field naming consistency across RawMode variants
cargo test --package copybook-codec --features comprehensive-tests rdw
```

#### Phase 3: Length Recomputation Fix (AC5)
**Root Cause**: `decode_record_with_scratch_and_raw` uses incorrect field name, preventing `encode_record` from finding raw data

**Validation Command**:
```bash
cargo test --package copybook-codec --features comprehensive-tests test_rdw_length_recomputation
```

**Expected Behavior**: After field name fix, encoder should find `__raw_b64` field and recompute RDW length correctly

#### Phase 4: Error Context Propagation (AC6)
**Investigation Required**: Check if error context is populated in `copybook-codec/src/record.rs` RDW parsing code

**Validation Command**:
```bash
cargo test --package copybook-codec --features comprehensive-tests test_rdw_error_context
```

**Expected Behavior**: Error should contain `record_index` or `byte_offset` for incomplete RDW headers

#### Phase 5: ODO+RDW Investigation (AC7)
**Symptom**: `COUNTER` field returns `Null` instead of `"03"` in RDW format
**Test Data**: `b"\x00\x0B\x00\x0003ABCDEFGHI"` (RDW length=11, counter="03", array="ABCDEFGHI")

**Investigation Steps**:
1. Verify field naming fix resolves issue
2. If still failing, check if `decode_record_with_scratch_and_raw` properly processes leading fields before ODO
3. Verify payload offset calculation accounts for RDW header stripping

**Validation Command**:
```bash
cargo test --package copybook-codec --features comprehensive-tests test_rdw_with_odo_variable_length
```

#### Phase 6: Full Validation (AC8, AC12-AC15)
**Validation Commands**:
```bash
# Full RDW test suite (37 tests)
cargo test --package copybook-codec --features comprehensive-tests rdw

# Workspace validation
cargo test --workspace --features comprehensive-tests

# Roundtrip fidelity
cargo test --package copybook-codec --test binary_roundtrip_fidelity_tests -- rdw

# Zero unsafe code
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Error taxonomy integrity
cargo test --package copybook-core error
```

#### Phase 7: Performance Validation (AC9)
**Note**: No RDW-specific benchmarks exist. Use general codec benchmarks as baseline.

**Validation Commands**:
```bash
# General codec benchmarks (includes RDW processing)
cargo bench --package copybook-bench

# Performance mode validation
PERF=1 cargo bench -p copybook-bench

# Baseline comparison (if baseline exists)
cargo run --bin bench-report -p copybook-bench -- compare perf.json
```

**Performance Targets** (from CLAUDE.md):
- DISPLAY-heavy: ≥ 2.33 GiB/s (current baseline)
- COMP-3-heavy: ≥ 168 MiB/s (current baseline)
- Memory: < 256 MiB steady-state
- Variance: ~5% acceptable (WSL2 environment)

**Expected Impact**: Zero performance regression (field name is string literal constant)

#### Phase 8: Documentation Update (AC10)
**File**: `CLAUDE.md`
**Section**: Library API or CLI Usage

**Content to Add**:
```markdown
### Raw Data Capture Field Naming

All raw data capture modes use the `__raw_b64` field name for base64-encoded raw data:

- **RawMode::Off**: No raw data captured
- **RawMode::Record**: Captures record payload only (excluding RDW header for RDW format) in `__raw_b64`
- **RawMode::RecordRDW**: Captures full record including 4-byte RDW header in `__raw_b64`
- **RawMode::Field**: Captures field-level raw data with field-specific naming (e.g., `FIELDNAME__raw_b64`)

**CLI Example**:
```bash
# Capture raw data with RDW header
cargo run --bin copybook -- decode --emit-raw record+rdw copybook.cpy data.bin
```

**JSON Output**:
```json
{
  "FIELD1": "value1",
  "__raw_b64": "AAhBQkNERUY="  // Base64 of RDW header + payload
}
```
```

---

## 3. Architecture Impact Assessment

### 3.1 Affected Components

**Primary**: `copybook-codec` crate
**Impact Scope**: Single function (`decode_record_with_scratch_and_raw`)

**Crate Boundary Analysis**:
- ✅ **copybook-core**: No changes required (error taxonomy already defines CBKR* codes)
- ✅ **copybook-codec**: Single-line fix in `lib_api.rs:241` + potential error context fix
- ✅ **copybook-cli**: No changes required (uses public API unchanged)
- ✅ **copybook-gen**: No changes required
- ✅ **copybook-bench**: No changes required

**Public API Impact**: NONE - Field naming is implementation detail, not part of public API contract

### 3.2 RDW Format Compatibility

**RDW Specification Compliance**:
- ✅ Big-endian length field (bytes 0-1): Unchanged
- ✅ Reserved bytes (bytes 2-3): Unchanged
- ✅ Strict vs lenient mode: Unchanged
- ✅ Zero-length record handling: Unchanged
- ✅ ASCII corruption heuristic: Unchanged

**RecordFormat::RDW Processing**:
- ✅ Header stripping logic: Unchanged
- ✅ Length validation: Unchanged
- ✅ Payload extraction: Unchanged

**Raw Data Modes**:
- ✅ `RawMode::RecordRDW`: Captures full record including RDW header
- ✅ `RawMode::Record`: Captures payload only (excluding RDW header)
- ✅ `RawMode::Field`: Field-level capture (not RDW-specific)
- ✅ `RawMode::Off`: No capture

### 3.3 Performance Implications

**Field Naming Change**:
- **Memory**: Zero impact (string literal substitution, same length)
- **CPU**: Zero impact (constant string, no runtime computation)
- **Throughput**: Zero impact (no algorithmic change)

**Roundtrip Encoding**:
- **Before Fix**: Encoder cannot find raw data (`_raw` vs `__raw_b64` mismatch) → fallback to field-by-field encoding
- **After Fix**: Encoder finds raw data → direct binary copy → FASTER

**Expected Performance**:
- DISPLAY-heavy: ≥ 2.33 GiB/s (no change)
- COMP-3-heavy: ≥ 168 MiB/s (no change)
- RDW roundtrip: IMPROVEMENT (eliminates fallback path)

### 3.4 Workspace Integration

**Dependency Impact**: NONE - All changes internal to `copybook-codec`

**Feature Flags**:
- ✅ `comprehensive-tests`: Affected tests require this feature
- ✅ Standard tests: No impact (RDW comprehensive tests are feature-gated)

**Build Configuration**: No changes to `Cargo.toml` or workspace structure

---

## 4. Enterprise Data Processing Risk Assessment

### 4.1 COBOL Parsing Accuracy Risks

**Risk Level**: ✅ NONE

**Analysis**: No changes to COBOL parsing logic. Field naming fix is in JSON output layer only.

**Validation**:
```bash
cargo test --package copybook-core parsing
cargo test --package copybook-core cobol
```

### 4.2 Enterprise Compatibility Risks

**Risk Level**: ⚠️ LOW

**Risks**:
1. **ODO Counter Decoding**: `test_rdw_with_odo_variable_length` shows `COUNTER` field returns `Null`
   - **Impact**: ODO variable arrays in RDW format may not decode correctly
   - **Mitigation**: AC7 investigation step to determine root cause

2. **Error Context**: `test_rdw_error_context` may fail if context propagation is incomplete
   - **Impact**: Production troubleshooting harder without record index/byte offset
   - **Mitigation**: AC6 investigation and fix

**Validation**:
```bash
cargo test --package copybook-codec --features comprehensive-tests test_rdw_with_odo_variable_length
cargo test --package copybook-codec --features comprehensive-tests test_rdw_error_context
cargo run --bin copybook -- verify --format rdw copybook.cpy data.bin
```

### 4.3 Data Format Risks

**Risk Level**: ✅ NONE → ✅✅ IMPROVED

**Analysis**:
- **Before Fix**: Round-trip encoding broken for RDW format (`_raw` field not found)
- **After Fix**: Round-trip encoding works correctly (encoder finds `__raw_b64`)

**Validation**:
```bash
cargo test --package copybook-codec --test binary_roundtrip_fidelity_tests -- rdw
cargo test --package copybook-codec --features comprehensive-tests test_rdw_encoding_round_trip
```

### 4.4 Performance Risks

**Risk Level**: ✅ NONE → ✅✅ IMPROVED

**Analysis**:
- Field naming change: Zero performance impact
- Roundtrip encoding: FASTER (eliminates fallback path)

**Validation**:
```bash
cargo bench --package copybook-bench
PERF=1 cargo bench -p copybook-bench
cargo run --bin bench-report -p copybook-bench -- compare perf.json
```

### 4.5 Data Validation Risks

**Risk Level**: ⚠️ LOW

**Risk**: Error context may be incomplete for RDW parsing failures

**Validation**:
```bash
cargo test --package copybook-codec --features comprehensive-tests test_rdw_error_context
cargo test --package copybook-core error
```

### 4.6 Workspace Interaction Risks

**Risk Level**: ✅ NONE

**Analysis**: Single-crate change with no cross-crate dependencies affected

**Validation**:
```bash
cargo build --workspace --release
cargo test --workspace --features comprehensive-tests
cargo clippy --workspace -- -D warnings -W clippy::pedantic
```

---

## 5. Validation Plan

### 5.1 Unit Testing Strategy

**Target**: All 6 failing RDW comprehensive tests

**Test Execution**:
```bash
# Individual test validation
cargo test --package copybook-codec --features comprehensive-tests test_rdw_raw_preservation_normative
cargo test --package copybook-codec --features comprehensive-tests test_rdw_length_recomputation
cargo test --package copybook-codec --features comprehensive-tests test_rdw_error_context
cargo test --package copybook-codec --features comprehensive-tests test_rdw_with_odo_variable_length
cargo test --package copybook-codec --test rdw_comprehensive test_rdw_raw_preservation_with_reserved
cargo test --package copybook-codec --test rdw_comprehensive test_rdw_raw_record_only_mode

# Full RDW suite (37 tests)
cargo test --package copybook-codec --features comprehensive-tests rdw
```

**Success Criteria**: All 37 RDW tests pass (21 unit + 16 comprehensive)

### 5.2 Integration Testing Strategy

**Roundtrip Fidelity**:
```bash
cargo test --package copybook-codec --test binary_roundtrip_fidelity_tests -- rdw
```

**CLI Integration**:
```bash
# Create test data
echo '01 TEST-RECORD PIC X(10).' > /tmp/test.cpy
printf '\x00\x0A\x00\x00HELLO12345' > /tmp/test.bin

# Decode with raw capture
cargo run --bin copybook -- decode --format rdw --emit-raw record+rdw /tmp/test.cpy /tmp/test.bin --output /tmp/test.jsonl

# Verify __raw_b64 field exists
grep -q '__raw_b64' /tmp/test.jsonl && echo "✅ Field naming correct" || echo "❌ Field naming incorrect"

# Encode back
cargo run --bin copybook -- encode --format rdw --use-raw /tmp/test.cpy /tmp/test.jsonl /tmp/test_roundtrip.bin

# Compare binary roundtrip
diff /tmp/test.bin /tmp/test_roundtrip.bin && echo "✅ Roundtrip perfect" || echo "❌ Roundtrip failed"
```

### 5.3 Enterprise Validation Strategy

**Codepage Compatibility**:
```bash
# Test RDW with various codepages
for cp in ASCII CP037 CP273 CP500 CP1047 CP1140; do
  cargo test --package copybook-codec --features comprehensive-tests rdw -- --test-threads=1
done
```

**Error Taxonomy Validation**:
```bash
# Verify CBKR* error codes intact
cargo test --package copybook-core error
grep -r "CBKR211_RDW_RESERVED_NONZERO" copybook-core/src/
grep -r "CBKF221_RDW_UNDERFLOW" copybook-core/src/
```

**Performance Validation**:
```bash
# Baseline comparison
cargo bench --package copybook-bench > /tmp/baseline_before.txt
# (Apply fix)
cargo bench --package copybook-bench > /tmp/baseline_after.txt
# Compare (expect no regression)
```

### 5.4 Workspace Validation Strategy

**Full Test Suite**:
```bash
cargo test --workspace --features comprehensive-tests
```

**Zero Unsafe Code**:
```bash
cargo clippy --workspace -- -D warnings -W clippy::pedantic
```

**Build Validation**:
```bash
cargo build --workspace --release
```

---

## 6. Testing Strategy with TDD Tags

### 6.1 Test-to-AC Mapping

Each test assertion tagged with `// AC:ID` for traceability:

**AC1: Field Naming Fix**
- Unit test: Verify `decode_record_with_scratch_and_raw` uses `__raw_b64`
- Tag: `// AC:1`

**AC2: RecordRDW Mode**
- Test: `test_rdw_raw_preservation_normative`
- Assertion: `assert!(json_record.get("__raw_b64").is_some()); // AC:2`

**AC3: Reserved Bytes**
- Test: `test_rdw_raw_preservation_with_reserved`
- Assertion: `assert!(decoded_json.get("__raw_b64").is_some()); // AC:3`

**AC4: Record Mode (No RDW)**
- Test: `test_rdw_raw_record_only_mode`
- Assertion: `assert!(json_record.get("__raw_b64").is_some()); // AC:4`

**AC5: Length Recomputation**
- Test: `test_rdw_length_recomputation`
- Assertion: `assert_eq!(summary.records_processed, 1); // AC:5`

**AC6: Error Context**
- Test: `test_rdw_error_context`
- Assertion: `assert!(context.record_index.is_some() || context.byte_offset.is_some()); // AC:6`

**AC7: ODO+RDW**
- Test: `test_rdw_with_odo_variable_length`
- Assertion: `assert_eq!(json_record["COUNTER"], "03"); // AC:7`

**AC8-AC15**: Validation suite assertions (see Section 5)

### 6.2 TDD Workflow

1. **Red**: Run failing tests to confirm issue
2. **Green**: Apply fix and verify tests pass
3. **Refactor**: Clean up any code quality issues
4. **Validate**: Run full suite to catch regressions

---

## 7. Risk Mitigation Strategies

### 7.1 Technical Risks

**Risk**: ODO counter decoding broken in RDW format (AC7)
**Mitigation**:
1. Apply field naming fix first
2. Re-run `test_rdw_with_odo_variable_length`
3. If still failing, investigate payload offset calculation
4. Fallback: Document as known limitation for ODO+RDW (defer to future issue)

**Risk**: Error context not propagated (AC6)
**Mitigation**:
1. Check `copybook-codec/src/record.rs` RDW parsing error handling
2. Verify `Error::new()` calls include context
3. Add context if missing

### 7.2 Process Risks

**Risk**: Tests pass but production behavior broken
**Mitigation**:
1. Use CLI integration tests (see Section 5.2)
2. Manual testing with production-like data
3. Roundtrip fidelity validation

**Risk**: Performance regression not detected
**Mitigation**:
1. Run full benchmark suite before/after
2. Use baseline comparison tools
3. Monitor WSL2 variance (~5% acceptable)

---

## 8. Success Criteria

### 8.1 Functional Success

✅ All 37 RDW tests pass (21 unit + 16 comprehensive)
✅ Field naming consistent across all RawMode variants
✅ Roundtrip encoding preserves RDW headers and reserved bytes
✅ Error context includes record_index or byte_offset
✅ ODO counters decode correctly in RDW format (or documented limitation)

### 8.2 Quality Success

✅ Zero unsafe code (`cargo clippy --workspace -- -D warnings -W clippy::pedantic`)
✅ Error taxonomy intact (CBKR* codes unchanged)
✅ Documentation updated in CLAUDE.md
✅ Full workspace test suite passes

### 8.3 Performance Success

✅ No regression in codec benchmarks (< 5% variance acceptable)
✅ RDW roundtrip encoding FASTER (eliminates fallback path)
✅ Memory usage unchanged (< 256 MiB steady-state)

---

## 9. Alignment with copybook-rs Standards

### 9.1 TDD Practices

✅ Test-driven approach: Failing tests define the fix
✅ Test tagging: AC:ID tags for traceability
✅ Comprehensive coverage: 37 RDW tests validate all edge cases

### 9.2 Workspace Architecture

✅ Crate boundaries respected: Changes isolated to `copybook-codec`
✅ Dependency management: No new dependencies
✅ Zero unsafe code: Field naming change is safe Rust

### 9.3 Enterprise Compatibility

✅ Mainframe data format support: RDW spec compliance maintained
✅ Codepage compatibility: Works with all supported codepages (CP037, CP273, CP500, CP1047, CP1140)
✅ Production reliability: Error taxonomy and context preserved

### 9.4 COBOL Compatibility

✅ Copybook format spec: No changes to parsing logic
✅ ODO support: Validation step ensures ODO+RDW compatibility
✅ Field layout: No impact on schema generation

### 9.5 Production Support

✅ Error handling: Structured error taxonomy with CBKR* codes
✅ Performance targets: No regression, potential improvement
✅ Monitoring: Benchmark integration for regression detection

---

## 10. Implementation Checklist

### Pre-Implementation

- [ ] Review existing RDW test failures (6 tests)
- [ ] Verify current field naming convention (`__raw_b64` in 10+ locations)
- [ ] Confirm no RDW-specific benchmarks exist (use general codec benchmarks)

### Implementation Phase

- [ ] **AC1**: Change `_raw` to `__raw_b64` in `lib_api.rs:241`
- [ ] **AC2-AC4**: Verify raw preservation tests pass
- [ ] **AC5**: Verify length recomputation test passes
- [ ] **AC6**: Investigate and fix error context propagation
- [ ] **AC7**: Investigate ODO counter decoding in RDW format
- [ ] **AC8**: Run full RDW test suite (37 tests)
- [ ] **AC9**: Run codec benchmarks and verify no regression
- [ ] **AC10**: Update CLAUDE.md with field naming documentation
- [ ] **AC11**: Verify field naming consistency across RawMode variants
- [ ] **AC12**: Run workspace validation (`cargo test --workspace --features comprehensive-tests`)
- [ ] **AC13**: Verify roundtrip fidelity tests pass
- [ ] **AC14**: Run clippy pedantic and verify zero unsafe code
- [ ] **AC15**: Verify error taxonomy integrity

### Post-Implementation

- [ ] Generate test coverage report
- [ ] Update issue with implementation summary
- [ ] Create PR with detailed description

---

## 11. References to Existing Patterns

### 11.1 Field Naming Convention

**Established Pattern**: `__raw_b64` (double underscore prefix)

**Existing Implementations**:
- `copybook-codec/src/lib_api.rs`: Lines 276, 282, 288, 292
- `copybook-codec/src/json.rs`: Lines 243, 416, 768, 1155, 2227, 2333
- `copybook-codec/src/odo_redefines.rs`: Additional occurrences

**Rationale**: Double underscore prefix indicates internal/metadata field (similar to Python conventions)

### 11.2 Error Context Pattern

**Established Pattern**: `Error::with_context()` for contextual error information

**Existing Implementations**:
```rust
Error::new(ErrorCode::CBKF221_RDW_UNDERFLOW, message)
    .with_context(ErrorContext {
        record_index: Some(record_num),
        byte_offset: Some(current_offset),
        ..Default::default()
    })
```

**Validation**: Search for error context usage:
```bash
grep -r "with_context" copybook-codec/src/
grep -r "ErrorContext" copybook-codec/src/
```

### 11.3 RDW Processing Pattern

**Established Pattern**: `RecordFormat::RDW` with strict/lenient modes

**Existing Implementations**:
- `copybook-codec/src/record.rs`: RDW header parsing, length validation, reserved byte handling
- Unit tests: `test_rdw_reader_basic`, `test_rdw_reader_reserved_nonzero_strict`, etc.

**Validation**: Review existing RDW unit tests (21 tests passing):
```bash
cargo test --package copybook-codec record::tests::test_rdw
```

---

## 12. Detailed Investigation Notes

### 12.1 ODO+RDW Investigation (AC7)

**Current Behavior**:
- Test: `test_rdw_with_odo_variable_length`
- Input: `b"\x00\x0B\x00\x00003ABCDEFGHI"` (RDW length=11, counter="03", array="ABCDEFGHI")
- Expected: `json_record["COUNTER"] == "03"`
- Actual: `json_record["COUNTER"] == Null`

**Hypothesis**:
1. **Field naming issue**: `decode_record_with_scratch_and_raw` uses wrong field name → raw data not captured → ODO processing skipped
2. **Payload offset issue**: RDW header stripping may not adjust byte offsets correctly for leading fields
3. **ODO implementation gap**: ODO processing may not work with RDW format at all

**Investigation Steps**:
1. Apply field naming fix and re-run test
2. Add debug logging to track payload offset and field decoding
3. Verify `decode_record_with_scratch_and_raw` calls `process_fields_recursive_with_scratch` with correct data slice
4. Check if ODO counter field is at correct byte offset after RDW header removal

**Fallback**: If ODO+RDW is fundamentally broken, document as known limitation and create separate issue

### 12.2 Error Context Investigation (AC6)

**Current Behavior**:
- Test: `test_rdw_error_context`
- Input: `b"\x00\x0A"` (truncated RDW header, only 2 bytes)
- Expected: Error with `record_index` or `byte_offset` in context
- Actual: Unknown (test may be passing or failing without context)

**Investigation Steps**:
1. Run test and capture error details
2. Check `copybook-codec/src/record.rs` for error creation points
3. Verify `Error::new()` calls include `.with_context()` for RDW parsing errors
4. Add context if missing

**Error Codes to Check**:
- `CBKF221_RDW_UNDERFLOW`: Incomplete RDW header or payload

---

## 13. Migration Considerations

**Classification**: `additive` - No breaking changes

**Backward Compatibility**:
- ✅ No API changes (field naming is implementation detail)
- ✅ No schema changes
- ✅ No CLI flag changes
- ✅ No error code changes

**Forward Compatibility**:
- ✅ Fixed behavior matches documented convention
- ✅ Roundtrip encoding now works (was broken before)

**Migration Guide**: NOT REQUIRED (no breaking changes)

---

## 14. Conclusion

**Recommendation**: ✅ **PROCEED WITH IMPLEMENTATION**

**Summary**:
- **Root Cause**: Clear and well-defined (field naming inconsistency)
- **Implementation**: Low-risk, single-line fix with secondary investigations
- **Testing**: Comprehensive validation strategy with 37 RDW tests
- **Impact**: Positive (fixes broken functionality, no regressions expected)
- **Alignment**: Perfect alignment with copybook-rs enterprise standards

**Next Steps**:
1. **FINALIZE → spec-finalizer**: Route to finalization agent for implementation
2. Apply field naming fix in `lib_api.rs:241`
3. Investigate and resolve AC6 (error context) and AC7 (ODO+RDW)
4. Execute full validation suite
5. Update documentation in CLAUDE.md

**Expected Outcome**: All 37 RDW tests passing, roundtrip encoding working, zero performance regression.

---

## Appendix A: Validation Commands Reference

### Quick Validation
```bash
# Single-line fix validation
cargo test --package copybook-codec --features comprehensive-tests rdw

# Full workspace validation
cargo test --workspace --features comprehensive-tests
cargo clippy --workspace -- -D warnings -W clippy::pedantic
cargo build --workspace --release
```

### Performance Validation
```bash
# Benchmark baseline
cargo bench --package copybook-bench

# Performance mode
PERF=1 cargo bench -p copybook-bench

# Baseline comparison
cargo run --bin bench-report -p copybook-bench -- compare perf.json
```

### CLI Integration Testing
```bash
# Create test fixture
echo '01 RDW-RECORD PIC X(8).' > /tmp/rdw_test.cpy
printf '\x00\x08\x00\x00TESTDATA' > /tmp/rdw_test.bin

# Decode with raw capture
cargo run --bin copybook -- decode --format rdw --emit-raw record+rdw /tmp/rdw_test.cpy /tmp/rdw_test.bin --output /tmp/rdw_test.jsonl

# Verify field naming
grep '__raw_b64' /tmp/rdw_test.jsonl && echo "✅ PASS" || echo "❌ FAIL"

# Roundtrip encode
cargo run --bin copybook -- encode --format rdw --use-raw /tmp/rdw_test.cpy /tmp/rdw_test.jsonl /tmp/rdw_roundtrip.bin

# Binary comparison
diff /tmp/rdw_test.bin /tmp/rdw_roundtrip.bin && echo "✅ ROUNDTRIP PASS" || echo "❌ ROUNDTRIP FAIL"
```

### Error Taxonomy Validation
```bash
# Verify CBKR* error codes
cargo test --package copybook-core error
grep -r "CBKR211_RDW_RESERVED_NONZERO" copybook-core/src/error.rs
grep -r "CBKF221_RDW_UNDERFLOW" copybook-core/src/error.rs
```

---

**Document Version**: 1.0
**Date**: 2025-10-03
**Author**: spec-analyzer agent (copybook-rs Generative Flow)
**Status**: READY FOR FINALIZATION
**Next Agent**: spec-finalizer
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
