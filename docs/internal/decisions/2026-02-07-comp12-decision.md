<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# COMP-1/COMP-2 Decision Gate Document

**Document Version**: 1.1
**Decision Date**: 2026-02-07
**Status**: **Superseded** — COMP-1/2 implemented and promoted to stable (v0.4.3)
**Related Issue**: Phase 3.3 of Implementation Plan

> **Supersession Notice**: This decision was overtaken by implementation.
> COMP-1 and COMP-2 floating-point support is now fully implemented, tested,
> and default-enabled. References:
>
> - **Current support status**: [`docs/reference/COBOL_SUPPORT_MATRIX.md`](../../reference/COBOL_SUPPORT_MATRIX.md)
> - **Parse tests**: [`copybook-core/tests/comp_float_parse_tests.rs`](../../../copybook-core/tests/comp_float_parse_tests.rs)
> - **Codec tests**: [`copybook-codec/tests/comp_float_codec_tests.rs`](../../../copybook-codec/tests/comp_float_codec_tests.rs)
> - **Default-enabled flag**: [`copybook-core/src/feature_flags.rs` (line 181)](../../../copybook-core/src/feature_flags.rs)

---

## Executive Summary

**Decision**: **DO NOT IMPLEMENT** COMP-1/COMP-2 floating-point support at this time.

This decision is based on comprehensive analysis of:
- COBOL environment specifications and endianness requirements
- Availability of real-world test fixtures
- Implementation complexity and validation challenges
- User demand and enterprise use case analysis
- Risk assessment for incorrect implementation

The feature will remain explicitly unsupported with clear documentation and stable rejection behavior.

---

## 1. Decision Checklist - Research Findings

### 1.1 Target Encoding and Endianness

#### COBOL COMP-1 Specifications

**What COBOL environments use COMP-1?**
- **IBM Enterprise COBOL**: COMP-1 is single-precision floating-point (32-bit IEEE 754)
- **Micro Focus COBOL**: COMP-1 is single-precision floating-point (32-bit IEEE 754)
- **GNU COBOL (GnuCOBOL)**: COMP-1 is single-precision floating-point (32-bit IEEE 754)

**Endianness requirements for COMP-1:**
- **IBM z/Architecture (mainframe)**: Big-endian byte order
- **IBM Enterprise COBOL on x86**: Uses big-endian representation (preserves mainframe compatibility)
- **Micro Focus on x86**: Uses little-endian representation (native platform)
- **GnuCOBOL**: Uses native platform endianness (little-endian on x86)

**Platform-specific considerations:**
- Mainframe-generated data will be big-endian IEEE 754
- x86-generated data may be little-endian (Micro Focus, GnuCOBOL)
- Byte-swapping may be required for cross-platform compatibility
- Hexadecimal floating-point formats exist on some legacy IBM systems (not IEEE 754)

#### COBOL COMP-2 Specifications

**What COBOL environments use COMP-2?**
- **IBM Enterprise COBOL**: COMP-2 is double-precision floating-point (64-bit IEEE 754)
- **Micro Focus COBOL**: COMP-2 is double-precision floating-point (64-bit IEEE 754)
- **GNU COBOL (GnuCOBOL)**: COMP-2 is double-precision floating-point (64-bit IEEE 754)

**Endianness requirements for COMP-2:**
- Same endianness rules as COMP-1 apply
- 64-bit alignment requirements on some platforms
- Potential padding bytes for alignment in record layouts

**Summary**: COMP-1/COMP-2 are IEEE 754 floating-point types, but endianness varies by platform and compiler. This creates significant validation complexity.

---

### 1.2 Real Fixtures Availability

#### Current Fixture Status

**Do we have real copybook + raw bytes + expected numeric samples?**
- **NO** - No real-world COMP-1/COMP-2 fixtures exist in the project
- Only test fixtures that reference COMP-1/COMP-2 exist as negative tests (expected to fail)
- Example from `golden_fixtures_ac4_sibling_after_odo_fail.rs`:
  ```cobol
  10 OFFSET-VALUE    PIC S9(5)V9(8) COMP-1.
  10 SCALE-FACTOR    PIC 9V9(10) COMP-2.
  ```
  These fixtures are designed to test ODO tail constraints, not COMP-1/COMP-2 functionality.

**What would be required to obtain real fixtures?**
1. **Mainframe data access**: Access to IBM mainframe systems with COMP-1/COMP-2 data
2. **Copybook samples**: Real production copybooks using COMP-1/COMP-2
3. **Raw byte dumps**: Binary data files with known COMP-1/COMP-2 values
4. **Expected values**: Reference values from mainframe processing (for validation)
5. **Multiple sources**: Data from different COBOL compilers to test endianness variations
6. **Edge cases**: Special values (infinity, NaN, denormalized numbers, overflow, underflow)

**Can we generate synthetic fixtures that are representative?**
- **PARTIALLY** - We can generate IEEE 754 floating-point bytes using Rust's `f32` and `f64` types
- **LIMITATIONS**:
  - Cannot guarantee representation matches specific COBOL compiler output
  - Cannot test cross-platform endianness without real data
  - Cannot validate against mainframe reference values
  - Edge cases (NaN, infinity) may have different bit patterns across implementations

**Summary**: Lack of real-world fixtures is a critical blocker. Synthetic fixtures are insufficient for validating correctness across platforms.

---

### 1.3 Numeric Tolerances and Representation

#### Acceptable Numeric Tolerances

**What are acceptable numeric tolerances for COMP-1/COMP-2?**
- **COMP-1 (f32)**: Approximately 7 decimal digits of precision, epsilon ≈ 1.19e-7
- **COMP-2 (f64)**: Approximately 15 decimal digits of precision, epsilon ≈ 2.22e-16
- **Tolerance for comparison**: Machine epsilon is the natural tolerance for floating-point comparison
- **No universal tolerance**: Different applications may require different precision guarantees

**How should these be represented in JSON?**
- **Option 1**: Native JSON numbers (limited to 64-bit floating-point per JSON spec)
- **Option 2**: String representation (preserves exact decimal representation)
- **Option 3**: Structured representation with metadata (e.g., `{"value": 1.23, "type": "COMP-1"}`)

**Recommendation**: Native JSON numbers for simplicity, but this has implications:
- COMP-1 values can be represented losslessly in JSON numbers
- COMP-2 values may lose precision due to JSON's 64-bit limit
- Round-trip fidelity cannot be guaranteed for all COMP-2 values

#### Edge Cases

**Overflow**: Values exceeding the range of the floating-point type
- **COMP-1**: ±3.4e38
- **COMP-2**: ±1.8e308
- **Handling**: Should produce error or special value (infinity)

**Underflow**: Values too small to represent (subnormal numbers)
- **COMP-1**: ±1.4e-45
- **COMP-2**: ±5.0e-324
- **Handling**: Gradual underflow to subnormal numbers, then to zero

**NaN (Not a Number)**: Invalid floating-point values
- **Bit patterns**: Multiple representations of NaN exist
- **Handling**: JSON has no NaN representation; must use `null` or string
- **Round-trip**: NaN cannot be round-tripped through JSON natively

**Infinity**: Positive and negative infinity
- **Bit patterns**: Specific IEEE 754 representations
- **Handling**: JSON has no infinity representation; must use `null`, string, or structured format
- **Round-trip**: Infinity cannot be round-tripped through JSON natively

**Summary**: Floating-point edge cases create significant round-trip fidelity challenges, conflicting with copybook-rs's core design principle of lossless round-trip.

---

## 2. Decision Criteria Analysis

### 2.1 Availability of Real-World Samples

| Criterion | Status | Impact |
|-----------|--------|---------|
| Real copybooks with COMP-1/COMP-2 | ❌ None available | Critical blocker |
| Raw binary data with known values | ❌ None available | Critical blocker |
| Mainframe reference values | ❌ None available | Critical blocker |
| Cross-platform test data | ❌ None available | Critical blocker |
| Edge case data (NaN, infinity) | ❌ None available | Critical blocker |

**Assessment**: **FAIL** - No real-world samples available. Implementation would be unvalidated.

---

### 2.2 Complexity of Implementation

| Aspect | Complexity | Risk |
|--------|------------|------|
| IEEE 754 parsing (f32/f64) | Medium | Low |
| Endianness handling (big/little) | High | Medium |
| Alignment and padding | Medium | Low |
| JSON representation | High | High |
| Edge case handling (NaN, infinity) | High | High |
| Round-trip fidelity | Very High | Critical |
| Cross-platform compatibility | Very High | Critical |

**Assessment**: **HIGH COMPLEXITY** - Multiple high-risk areas, especially round-trip fidelity.

---

### 2.3 Risk of Incorrect Implementation

| Risk Type | Probability | Impact | Mitigation |
|-----------|-------------|---------|------------|
| Endianness mismatch | High | High | Real fixtures required |
| NaN/infinity round-trip failure | High | High | JSON limitation |
| Precision loss in JSON | Medium | Medium | String representation |
| Alignment miscalculation | Low | Medium | Test coverage |
| Subtle IEEE 754 edge cases | Medium | High | Comprehensive testing |

**Assessment**: **HIGH RISK** - Multiple high-probability, high-impact risks exist.

---

### 2.4 Demand from Users

| Evidence | Status |
|----------|--------|
| GitHub issues requesting COMP-1/COMP-2 | None found |
| Enterprise customer requests | None documented |
| Migration guide mentions COMP-1/COMP-2 | As unsupported feature |
| Production use cases | None reported |
| Alternative patterns documented | Use COMP-3 instead |

**Assessment**: **LOW DEMAND** - No explicit user demand documented. Migration guide recommends COMP-3 as alternative.

---

## 3. Decision: DO NOT IMPLEMENT

### 3.1 Rationale

Based on the decision criteria analysis:

1. **No Real-World Fixtures**: Critical blocker - cannot validate correctness without real data
2. **High Implementation Complexity**: Multiple high-risk areas, especially round-trip fidelity
3. **High Risk of Incorrect Implementation**: Endianness, JSON representation, edge cases
4. **Low User Demand**: No documented requests or production use cases
5. **Existing Alternatives**: COMP-3 (packed decimal) provides decimal precision without floating-point issues
6. **Design Principle Conflict**: Lossless round-trip is a core principle; floating-point cannot guarantee this

### 3.2 Supporting Evidence

From project documentation:

- **COBOL_SUPPORT_MATRIX.md**: "Single/double float - by design, not implemented"
- **USER_GUIDE.md**: "COMP-1/COMP-2 floating-point (rare in production)"
- **MIGRATION_GUIDE.md**: Recommends replacing COMP-1/COMP-2 with COMP-3
- **ROADMAP.md**: Listed under "Feature Gaps" with "Not supported" status
- **internal/state-and-path.md**: "COMP-1/COMP-2 floats: Not in roadmap; rare in modern mainframes"

### 3.3 Future Considerations

The decision to not implement COMP-1/COMP-2 is **not permanent**. Reconsideration is appropriate if:

1. **Real-world fixtures become available**: Mainframe data with known values
2. **Explicit user demand emerges**: Enterprise customers with production requirements
3. **Use case is well-defined**: Clear understanding of precision and round-trip requirements
4. **Testing strategy is viable**: Plan for validating correctness across platforms

---

## 4. Implementation Plan (If Implementing in Future)

This section is retained for future reference should the decision be reconsidered.

### 4.1 Feature Flag Governance

**Flag Keys**:
- `Comp1` - Enable COMP-1 (single precision floating point) support
- `Comp2` - Enable COMP-2 (double precision floating point) support

**Default State**: Disabled (false)

**Owner**: TBD

**Rollout Plan**:
1. **Phase 1**: Enable for internal testing with synthetic fixtures
2. **Phase 2**: Enable for beta users with real-world fixtures
3. **Phase 3**: Gradual promotion to production users
4. **Phase 4**: Remove feature flag once stable

**Kill-Switch Behavior**: Reject with appropriate error code (`CBKP011_UNSUPPORTED_CLAUSE`)

**Cleanup Milestone**: v1.6.0 (or appropriate)

### 4.2 Format/Dialect Requirements

**COMP-1 (Single Precision)**:
- Size: 4 bytes (32 bits)
- Format: IEEE 754 single-precision
- Alignment: 4-byte aligned on most platforms
- Endianness: Configurable (default: big-endian for mainframe compatibility)

**COMP-2 (Double Precision)**:
- Size: 8 bytes (64 bits)
- Format: IEEE 754 double-precision
- Alignment: 8-byte aligned on most platforms
- Endianness: Configurable (default: big-endian for mainframe compatibility)

**Configuration**:
```toml
[endianness]
comp_floats = "big"  # or "little", "native"
```

### 4.3 Fixtures Required

**Minimum Fixture Set**:
1. **Basic values**: Zero, positive, negative numbers
2. **Precision tests**: Values at precision boundaries
3. **Edge cases**: NaN, infinity, subnormal numbers
4. **Overflow/underflow**: Values exceeding range
5. **Cross-platform**: Same values from different COBOL compilers
6. **Real-world**: Production copybooks with COMP-1/COMP-2

**Fixture Format**:
```json
{
  "copybook": "...",
  "raw_bytes": "base64-encoded",
  "expected_values": {
    "COMP1_FIELD": 1.23,
    "COMP2_FIELD": 4.56
  },
  "source_system": "IBM Enterprise COBOL",
  "endianness": "big"
}
```

### 4.4 Property Test Requirements

**Within Well-Defined Numeric Tolerances**:
```rust
proptest! {
    #[test]
    fn test_comp1_roundtrip(value in any::<f32>()) {
        // Test round-trip with tolerance = machine epsilon
        let tolerance = f32::EPSILON;
        // ... implementation
    }

    #[test]
    fn test_comp2_roundtrip(value in any::<f64>()) {
        // Test round-trip with tolerance = machine epsilon
        let tolerance = f64::EPSILON;
        // ... implementation
    }
}
```

**Special Cases**:
- NaN round-trip (multiple representations)
- Infinity round-trip (positive/negative)
- Subnormal numbers
- Denormalized numbers

### 4.5 Fuzzing Requirements

**Numeric Decode Fuzz Target Extended**:
```rust
// fuzz/fuzz_targets/comp_float_decode.rs
pub fn fuzz_comp_float_decode(data: &[u8]) {
    // Fuzz COMP-1/COMP-2 decoding
    // Test with arbitrary byte patterns
    // Validate no panics, no undefined behavior
}
```

**Coverage**:
- Arbitrary bit patterns (including invalid IEEE 754)
- Endianness variations
- Alignment variations
- Partial reads (insufficient bytes)

### 4.6 Mutation Testing Scope

**Numeric Float Paths**:
- IEEE 754 parsing logic
- Endianness conversion
- JSON serialization/deserialization
- Edge case handling (NaN, infinity)
- Precision loss detection
- Error handling for invalid values

**Target Coverage**: 75% for float-related code paths

---

## 5. Documentation (If Not Implementing)

### 5.1 Update Support Matrix

**COBOL_SUPPORT_MATRIX.md**:
```markdown
| COMP-1/COMP-2 (`comp-1-comp-2`) | ❌ Not Supported | N/A | Single/double float - by design, not implemented |
```

### 5.2 Document as Explicitly Unsupported

**USER_GUIDE.md**:
```markdown
### Known Limitations (By Design)
- COMP-1/COMP-2 floating-point (rare in production)
  - Use COMP-3 (packed decimal) for decimal precision
  - Use BINARY (COMP) for integer values
  - See MIGRATION_GUIDE.md for conversion examples
```

### 5.3 Document Error Codes for Rejection

**ERROR_CODES.md**:
```markdown
Error: CBKP011_UNSUPPORTED_CLAUSE
Clause: COMP-1
Field: ROOT.CUSTOMER.BALANCE
Message: Unsupported COBOL clause: COMP-1
Remediation: Replace COMP-1 with COMP-3 for decimal precision or BINARY for integers
```

### 5.4 Provide Migration Path

**MIGRATION_GUIDE.md**:
```markdown
### COMP-1/COMP-2 Floating Point

**Problem:** copybook-rs doesn't support floating point types.

**Solution:** Replace with supported alternatives:

// Before
05 RATE PIC S9(3)V99 COMP-1.
05 PRECISION PIC S9(5)V9(15) COMP-2.

// After - Use COMP-3 for decimal precision
05 RATE PIC S9(3)V99 COMP-3.
05 PRECISION PIC S9(5)V9(15) COMP-3.

// After - Use BINARY for integers
05 COUNT PIC 9(9) COMP.
```

### 5.5 Troubleshooting Matrix

**TROUBLESHOOTING_MATRIX.md**:
```markdown
## COMP-1/COMP-2 Floating Point

**Symptoms:**
- "Unsupported COBOL clause: COMP-1"
- "Unsupported COBOL clause: COMP-2"
- "Feature not implemented"

**Causes:**
- Copybook uses COMP-1 or COMP-2 floating-point types
- These types are not supported by copybook-rs

**Solutions:**

| Original | Replacement | Example |
|----------|-------------|----------|
| COMP-1 | COMP-3 | `PIC S9(3)V99 COMP-1` → `PIC S9(3)V99 COMP-3` |
| COMP-2 | COMP-3 | `PIC S9(5)V99 COMP-2` → `PIC S9(5)V99 COMP-3` |
```

---

## 6. Issues Discovered During Decision Process

### 6.1 Existing Test Fixtures Reference COMP-1/COMP-2

**Issue**: Test fixtures in `golden_fixtures_ac4_sibling_after_odo_fail.rs` reference COMP-1/COMP-2 fields but are designed to test ODO tail constraints, not COMP-1/COMP-2 functionality.

**Impact**: These tests may create confusion about COMP-1/COMP-2 support status.

**Recommendation**: Add comments to clarify that COMP-1/COMP-2 fields are used only for ODO testing and are not supported.

### 6.2 Feature Flags Defined But Unused

**Issue**: Feature flags `Comp1` and `Comp2` are defined in `feature_flags.rs` but have no implementation behind them.

**Impact**: May create false expectations about feature availability.

**Recommendation**: Keep feature flags defined for future use, but ensure documentation clearly states they are not implemented.

### 6.3 Support Matrix Inconsistency

**Issue**: Support matrix lists COMP-1/COMP-2 as "Not Supported" but feature flags exist, creating potential confusion.

**Recommendation**: Ensure all documentation consistently states "Not Supported" with clear migration guidance.

---

## 7. Conclusion

### 7.1 Final Decision

**DO NOT IMPLEMENT COMP-1/COMP-2 floating-point support at this time.**

### 7.2 Summary of Findings

| Criterion | Finding |
|-----------|----------|
| Real-world fixtures | ❌ None available |
| Implementation complexity | 🔴 High |
| Risk of incorrect implementation | 🔴 High |
| User demand | 🟢 Low |
| Alternative solutions | ✅ COMP-3 available |
| Design principle alignment | ❌ Conflicts with lossless round-trip |

### 7.3 Next Steps

1. **Update documentation** to explicitly state COMP-1/COMP-2 are not supported
2. **Document error codes** for rejection behavior
3. **Provide migration guidance** for users with COMP-1/COMP-2 copybooks
4. **Monitor for user demand** - reconsider if real use cases emerge
5. **Archive this decision document** for future reference

### 7.4 Reconsideration Criteria

Reconsider implementing COMP-1/COMP-2 if:
- Real-world fixtures become available from mainframe systems
- Enterprise customers request the feature with production use cases
- A viable testing strategy emerges for validating cross-platform correctness
- JSON representation limitations can be addressed without breaking round-trip fidelity

---

## Appendix A: COBOL COMP Type Reference

| Type | Description | Size | Format | Support Status |
|------|-------------|-------|---------|----------------|
| COMP / BINARY | Binary integer | 1/2/4/8 bytes | Two's complement | ✅ Fully Supported |
| COMP-1 | Single-precision float | 4 bytes | IEEE 754 | ❌ Not Supported |
| COMP-2 | Double-precision float | 8 bytes | IEEE 754 | ❌ Not Supported |
| COMP-3 | Packed decimal | Variable | BCD | ✅ Fully Supported |

## Appendix B: Related Documents

- [COBOL_SUPPORT_MATRIX.md](../../reference/COBOL_SUPPORT_MATRIX.md)
- [USER_GUIDE.md](../../USER_GUIDE.md)
- [MIGRATION_GUIDE.md](../../MIGRATION_GUIDE.md)
- [TROUBLESHOOTING_MATRIX.md](../../TROUBLESHOOTING_MATRIX.md)
- [ERROR_CODES.md](../../ERROR_CODES.md)
- [FEATURE_FLAGS.md](../../FEATURE_FLAGS.md)
- [ROADMAP.md](../../ROADMAP.md)

---

**Document End**
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
