# Issue #48: Binary round-trip encoding inconsistency in encode/decode cycle

## Context

The copybook-rs codec exhibits a critical data integrity issue in the encode/decode pipeline where zoned decimal fields lose their original binary encoding format during round-trip operations. This affects the **copybook-codec** crate's numeric processing module and impacts enterprise mainframe data workflows that require byte-perfect round-trip fidelity.

**Current Processing Pipeline Impact:**
- **Parse → Schema**: Unaffected - COBOL parsing works correctly
- **Decode**: Accepts both ASCII (0x30-0x39) and EBCDIC (0xF0-0xF9) digit zones
- **Encode**: Always outputs EBCDIC digit zones regardless of input format
- **JSON → Validation**: Logically identical but binary representation differs

**Affected Components:**
- `copybook-codec/src/numeric/zoned.rs` - Core zoned decimal processing
- `copybook-cli/src/commands/decode.rs` - CLI decode operations
- `copybook-cli/src/commands/encode.rs` - CLI encode operations
- Enterprise round-trip workflows requiring byte-perfect data preservation

**Data Integrity Risk:**
This inconsistency violates the fundamental expectation of binary round-trip fidelity essential for enterprise mainframe data processing, where original data format preservation is often legally or operationally required.

## User Story

As an **enterprise mainframe data engineer**, I want **zoned decimal fields to preserve their original binary encoding format (ASCII vs EBCDIC digit zones) during encode/decode round-trip cycles** so that **I can maintain byte-perfect data integrity required for regulatory compliance and seamless mainframe system integration**.

## Acceptance Criteria

**AC1**: Zoned decimal encoding format detection correctly identifies ASCII (0x30-0x39) vs EBCDIC (0xF0-0xF9) digit zones during decode operations

**AC2**: DecodeOptions supports `preserve_zoned_encoding: bool` flag that controls whether original encoding format is tracked and preserved

**AC3**: DecodeOptions supports `preferred_zoned_encoding` option with values `Ascii`, `Ebcdic`, and `Auto` for explicit encoding control

**AC4**: Encode operations use preserved encoding format when available, maintaining original ASCII or EBCDIC digit zones

**AC5**: CLI decode command supports `--preserve-encoding` flag that enables encoding format preservation

**AC6**: CLI encode command supports `--zoned-encoding {ascii|ebcdic|auto}` flag for explicit encoding format specification

**AC7**: Round-trip operations with `--preserve-encoding` flag produce byte-identical binary files when compared with `cmp` utility

**AC8**: Default behavior remains unchanged (EBCDIC output) for backward compatibility when preservation flags are not used

**AC9**: Mixed encoding detection correctly handles records where some zoned decimal fields use ASCII and others use EBCDIC

**AC10**: Performance impact of encoding detection and preservation is less than 5% compared to current throughput benchmarks (4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3)

**AC11**: Error handling provides clear CBKD* error codes for malformed zoned decimal fields with invalid digit zones

**AC12**: Round-trip test suite validates binary fidelity for both ASCII and EBCDIC zoned decimal input data using real COBOL copybook fixtures

## Technical Implementation Notes

**Affected Crates:**
- **copybook-codec**: Core zoned decimal encoding/decoding logic
- **copybook-cli**: Command-line interface and option parsing
- **copybook-core**: Schema metadata for encoding format tracking

**Pipeline Stages:**
- **Decode**: Enhanced with encoding format detection and preservation
- **Encode**: Modified to respect preserved or specified encoding format
- **JSON**: Schema metadata extended to track encoding format per field

**Performance Considerations:**
- Encoding detection adds minimal overhead to decode path
- Preserved format lookup optimizes encode path
- Enterprise targets maintained: 4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3

**COBOL Compatibility:**
- Supports both ASCII and EBCDIC zoned decimal representations
- Maintains compatibility with mainframe COBOL numeric field types
- Preserves original mainframe data format requirements

**Error Handling:**
- **CBKD001**: Invalid zoned decimal digit zone (not 0x30-0x39 or 0xF0-0xF9)
- **CBKD002**: Mixed encoding in single field detection failure
- **CBKD003**: Encoding format preservation buffer overflow

**Test Coverage:**
- Round-trip validation with real COBOL fixtures from `fixtures/`
- ASCII-only, EBCDIC-only, and mixed encoding scenarios
- Performance regression testing against current benchmarks
- Edge cases: all-zero fields, short fields, malformed data
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
