# COBOL Feature Support Matrix

**Last Updated**: 2025-10-22
**Version**: copybook-rs v0.3.1
**Canonical Reference**: This document is the authoritative source for COBOL feature support

## Support Status Legend

- ✅ **Fully Supported**: Comprehensive parse + codec + round-trip + negative tests
- ⚠️ **Partially Supported**: Parse support only, or limited test coverage
- ❌ **Not Supported**: Explicitly unsupported, may have rejection tests
- 🔄 **Planned**: On roadmap (see linked issues)

## Data Types

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| DISPLAY (PIC X) | ✅ Fully Supported | `zoned_encoding_format_tests.rs` (21 tests), `comprehensive_numeric_tests.rs` (15 tests) | Alphanumeric fields with EBCDIC/ASCII conversion |
| Zoned Decimal (PIC 9) | ✅ Fully Supported | `zoned_encoding_format_tests.rs` (11 tests), `comprehensive_numeric_tests.rs`, `cobol_fixture_zoned_encoding_tests.rs` (7 tests) | EBCDIC zones and ASCII overpunch with proper sign handling |
| COMP-3 (Packed Decimal) | ✅ Fully Supported | `comp3_property_tests.rs` (512+ property cases), `comp3_format_verification.rs`, `decimal_edge_cases.rs` (9 tests) | Nibble sign processing, edge cases, overflow/underflow |
| BINARY (COMP) | ✅ Fully Supported | `comprehensive_numeric_tests.rs`, `binary_roundtrip_fidelity_tests.rs` (11 tests) | Various widths: 1/2/4/8 bytes, signed/unsigned |
| COMP-1 (Single Float) | ❌ Not Supported | N/A | By design - not implemented |
| COMP-2 (Double Float) | ❌ Not Supported | N/A | By design - not implemented |
| Edited PIC (Z, /, $, etc.) | ❌ Not Supported | `inspect_edited_pic_fails.rs` | Negative rejection tests exist |

## Structural Features

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| ODO (OCCURS DEPENDING ON) | ✅ Fully Supported | `odo_comprehensive.rs` (21 tests), `golden_fixtures_odo.rs` (201 lines), `odo_tail_validation.rs` | Driver validation, tail constraints, payload length, clipping/raising |
| REDEFINES | ✅ Fully Supported | `redefines_comprehensive.rs` (18 tests, 20.5K), `comprehensive_redefines_odo_tests.rs` (16 tests) | Shorter/equal/longer overlays, encode ambiguity, raw preservation |
| Level-88 (Condition Values) | ✅ Fully Supported | `golden_fixtures_ac2_level88_after_odo.rs` (6 tests, 638 lines), `golden_fixtures_ac5_redefines_level88_interactions.rs` (8 tests, 838 lines) | Parse + codec with `FieldKind::Condition`, non-storage semantic validation |
| OCCURS (Fixed) | ✅ Fully Supported | Multiple test files | Fixed-size array support with 5+ dedicated tests |
| SYNCHRONIZED | ✅ Fully Supported | `comprehensive_parser_tests.rs` (22 tests) | Field alignment with padding calculation |
| BLANK WHEN ZERO | ✅ Fully Supported | Codec tests | 2+ tests for special value handling |
| Nested ODO | ❌ Not Supported | `golden_fixtures_ac4_sibling_after_odo_fail.rs` (9 negative tests) | By design - ODO within ODO not allowed |
| RENAMES (66-level) | ❌ Not Supported | N/A | Not implemented - see roadmap |

## Sign Handling

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| SIGN LEADING | ✅ Fully Supported | `zoned_encoding_format_tests.rs` | Standard zoned decimal with leading sign |
| SIGN TRAILING | ✅ Fully Supported | `zoned_encoding_format_tests.rs` | Standard zoned decimal with trailing sign |
| SIGN SEPARATE | ❌ Not Supported | N/A | See Issue #44 for planned implementation |
| Overpunch (EBCDIC/ASCII) | ✅ Fully Supported | `zoned_encoding_format_tests.rs`, `zoned_overpunch.rs` (8 tests) | Comprehensive overpunch with EBCDIC zones |

## Record Formats

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| Fixed-Length | ✅ Fully Supported | `record.rs` (35 tests), comprehensive test files | 8+ dedicated tests, streaming support |
| RDW (Variable-Length) | ✅ Fully Supported | `comprehensive_rdw_tests.rs` (15 tests, 18.7K), `rdw_comprehensive.rs` (11 tests) | Record Descriptor Word with streaming iterator |

## Codepages (EBCDIC)

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| CP037 (US/Canada) | ✅ Fully Supported | Primary codepage across 35+ test files | Default EBCDIC codepage |
| CP273 (German) | ✅ Fully Supported | 8+ tests in codec suite | Full character conversion support |
| CP500 (International) | ✅ Fully Supported | 8+ tests in codec suite | International variant support |
| CP1047 (Latinized) | ✅ Fully Supported | 8+ tests in codec suite | Latin-1 based EBCDIC |
| CP1140 (Euro) | ✅ Fully Supported | 8+ tests in codec suite | Euro currency support |
| ASCII (supplementary) | ✅ Fully Supported | 12+ tests in codec suite | For comparison/testing purposes |

## Error Code Coverage

Comprehensive error taxonomy with **23 discrete codes** tested across **664 test functions**:

### Parse Errors (CBKP*)
- `CBKP001_SYNTAX`: Copybook syntax errors
- `CBKP011_UNSUPPORTED_CLAUSE`: Unsupported COBOL clause or feature
- `CBKP021_ODO_NOT_TAIL`: ODO array not at tail position
- `CBKP051_UNSUPPORTED_EDITED_PIC`: Edited PIC clauses not supported
- Plus 4+ additional parse error codes

### Schema Validation Errors (CBKS*)
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field not found
- `CBKS141_RECORD_TOO_LARGE`: Record size exceeds maximum limit
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement (count > max)
- `CBKS302_ODO_RAISED`: ODO minimum value validation (count < min)

### Data Errors (CBKD*)
- `CBKD*`: 12+ codes for invalid decimals, truncated records, character conversion, numeric overflow

### Encode Errors (CBKE*)
- `CBKE*`: 3+ codes for type mismatches, bounds violations, encoding failures

See [ERROR_CODES.md](ERROR_CODES.md) for complete reference.

## Level-88 Condition Values - Support Status

**Status**: ✅ **Fully Supported** (parse + codec + structural validation)

**Evidence**:
- **Parse Support**: `FieldKind::Condition { values: Vec<String> }` in schema AST
- **Codec Support**: Non-storage semantic handling in decode/encode paths
- **Structural Validation**: Level-88 allowed after ODO (non-storage element)
- **Golden Fixtures**: 638 lines (`golden_fixtures_ac2_level88_after_odo.rs`) + 838 lines (`golden_fixtures_ac5_redefines_level88_interactions.rs`)
- **Test Results**: 6 passing tests in AC2, 8 passing tests in AC5
- **Enterprise Scenarios**: Healthcare, banking, inventory management with condition values

**API Integration**:
```rust
// Schema definition includes Level-88 condition values
pub enum FieldKind {
    Condition { values: Vec<String> },
    // ... other field types
}

// Codec handles Level-88 as non-storage elements
// Returns structured JSON representation for API consistency
FieldKind::Condition { values } => condition_value(values, "CONDITION")
```

**Layout Impact**: Level-88 fields consume zero bytes (`(0, 1u64)` in layout calculation), correctly implementing COBOL non-storage semantics.

**Known Limitations**: None. Full support for Level-88 condition values including complex interactions with ODO and REDEFINES.

## Test Infrastructure Summary

**Total Coverage**: **664 test functions** across **111 test files**

**Golden Fixtures Framework**:
- **4,375 lines** of golden fixture tests with SHA-256 verification
- **7 dedicated AC files** (AC1-AC8) covering enterprise production scenarios
- **Performance integration**: Automated regression detection with baselines
- **Structural validation**: ODO, Level-88, REDEFINES interactions comprehensively tested

**Test Categories**:
- Unit tests: ~16 per crate
- Integration tests: 79-231 per crate
- Property-based tests: 512+ cases for COMP-3
- Enterprise scenarios: Banking, insurance, retail, manufacturing, healthcare
- Panic elimination: Zero unsafe code, comprehensive error path coverage

See [TEST_INFRASTRUCTURE_LANDSCAPE.md](../TEST_INFRASTRUCTURE_LANDSCAPE.md) for detailed analysis.

## Adoption Guidance

### For Production Use

1. **Verify Feature Compatibility**: Ensure your copybooks only use ✅ Fully Supported features
2. **Test with Representative Data**: Use `copybook verify` command to validate against sample records
3. **Review Error Codes**: Familiarize with error taxonomy for production diagnostics
4. **Performance Validation**: Test with representative workload sizes (see [REPORT.md](../REPORT.md))

### Validation Workflow

```bash
# Parse copybook to verify syntax
cargo run --bin copybook -- parse your_copybook.cpy

# Inspect field layout
cargo run --bin copybook -- inspect your_copybook.cpy

# Verify data compatibility
cargo run --bin copybook -- verify --format fixed --codepage cp037 your_copybook.cpy sample_data.bin

# Decode sample records
cargo run --bin copybook -- decode your_copybook.cpy sample_data.bin --output test.jsonl --format fixed --codepage cp037
```

### For Evaluation

- ⚠️ **Partially Supported** features may work but lack comprehensive test coverage
- ❌ **Not Supported** features will be rejected during parsing with appropriate error codes
- 🔄 **Planned** features have roadmap timelines in [ROADMAP.md](../ROADMAP.md)

### Feature Request Process

If you require unsupported features:

1. Check [ROADMAP.md](../ROADMAP.md) for planned implementation timeline
2. Review existing GitHub Issues for feature discussions
3. Open new issue with enterprise use case and sample copybook
4. Provide representative data samples (anonymized) for testing

## Performance Standards

**CI Enforcement**: Performance workflow enforces throughput floors with regression detection

| Workload Type | Target | Achieved | Margin |
|---------------|--------|----------|--------|
| DISPLAY-heavy | ≥80 MiB/s | 205 MiB/s | 2.56x |
| COMP-3-heavy | ≥40 MiB/s | 58 MiB/s | 1.45x |
| Memory usage | <256 MiB | Validated | Steady-state for multi-GB files |

**Baseline**: Established 2025-09-30 (Commit 1fa63633) on WSL2/AMD Ryzen 9 9950X3D

See [REPORT.md](../REPORT.md) for complete performance analysis.

## References

- **Library API**: [LIBRARY_API.md](LIBRARY_API.md) - Comprehensive API documentation
- **CLI Reference**: [CLI_REFERENCE.md](../CLI_REFERENCE.md) - Command-line interface guide
- **Error Codes**: [ERROR_CODES.md](ERROR_CODES.md) - Complete error taxonomy
- **Test Infrastructure**: [TEST_INFRASTRUCTURE_LANDSCAPE.md](../TEST_INFRASTRUCTURE_LANDSCAPE.md) - Test coverage analysis
- **Golden Fixtures**: [CLAUDE.md](../../CLAUDE.md#golden-fixtures-system) - Fixture framework specification
- **Roadmap**: [ROADMAP.md](../ROADMAP.md) - Feature development timeline
- **Production Readiness**: [REPORT.md](../REPORT.md) - Enterprise deployment assessment

## Changelog

**2025-10-22**: Initial release
- Comprehensive feature matrix with test evidence
- Resolved Level-88 support contradiction (now ✅ Fully Supported)
- Added test infrastructure summary with 664 test function inventory
- Documented error code coverage (23 discrete codes)
- Performance standards with CI enforcement details
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
