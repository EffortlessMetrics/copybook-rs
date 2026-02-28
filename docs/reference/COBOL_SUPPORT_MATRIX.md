<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# COBOL Feature Support Matrix

**Last Updated**: 2026-02-15
**Version**: copybook-rs v0.4.3
**Canonical Reference**: This document is the authoritative source for COBOL feature support
**Historical Notes**: Status transitions and prior constraints are intentionally labeled as `[Historical: YYYY-MM-DD]` when included.

> 💡 **Tip**: You can query this matrix programmatically using the CLI:
> ```bash
> copybook support               # Display support matrix table
> copybook support --json        # Machine-readable JSON output
> copybook support --check level-88  # Validate specific feature (exit 0 if supported)
> ```

## Support Status Legend

- ✅ **Fully Supported**: Comprehensive parse + codec + round-trip + negative tests
- ⚠️ **Partially Supported**: Parse support only, or limited test coverage
- ❌ **Not Supported**: Explicitly unsupported, may have rejection tests
- 🔄 **Planned**: On roadmap (see linked issues)

## Data Types

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| DISPLAY (PIC X) | ✅ Fully Supported | `zoned_encoding_format_tests.rs::test_ascii_zoned_encoding_detection`, `comprehensive_numeric_tests.rs::test_alphanumeric_handling_normative`, `comprehensive_numeric_tests.rs::test_codepage_specific_behavior` | Alphanumeric fields with EBCDIC/ASCII conversion |
| Zoned Decimal (PIC 9) | ✅ Fully Supported | `zoned_encoding_format_tests.rs::test_ebcdic_zoned_encoding_detection`, `comprehensive_numeric_tests.rs::test_zoned_decimal_ebcdic_sign_zones_comprehensive`, `cobol_fixture_zoned_encoding_tests.rs::test_simple_fixture_zoned_encoding_integration` | EBCDIC zones and ASCII overpunch with proper sign handling |
| COMP-3 (Packed Decimal) | ✅ Fully Supported | `comp3_format_verification.rs::test_comp3_format_understanding`, `comprehensive_numeric_tests.rs::test_packed_decimal_comprehensive`, `decimal_edge_cases.rs::test_packed_zero_handling` (512+ property cases in `comp3_property_tests.rs`) | Nibble sign processing, edge cases, overflow/underflow |
| BINARY (COMP) | ✅ Fully Supported | `comprehensive_numeric_tests.rs::test_binary_signed_unsigned_edges`, `binary_roundtrip_fidelity_tests.rs::test_comp3_packed_decimal_roundtrip_accuracy`, `numeric_comprehensive.rs::test_binary_width_by_digits` | Various widths: 1/2/4/8 bytes, signed/unsigned |
| COMP-1/COMP-2 (`comp-1-comp-2`) | ✅ **Fully Supported** | `copybook-core/tests/comp_float_parse_tests.rs`, `copybook-codec/tests/comp_float_codec_tests.rs`, `copybook-codec/tests/comp_float_golden_tests.rs` | IEEE 754 and IBM HFP formats; always enabled (promoted from experimental) |
| Edited PIC (`edited-pic`) | ✅ **Fully Supported (E1/E2/E3)** | `edited_pic_e1_tests.rs` (15 tests), `edited_pic_decode_e2_tests.rs` (28 tests), `edited_pic_encode_e3_tests.rs` (160+ tests) | **E1**: Parse ✅ **E2**: Decode ✅ **E3**: Encode ✅ (see Edited PIC section below) |

## Structural Features

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| ODO (`occurs-depending`) | ✅ Fully Supported | `odo_comprehensive.rs::test_valid_odo_configuration`, `odo_comprehensive.rs::test_odo_payload_length_correctness`, `odo_counter_types.rs::test_odo_zoned_counter` | Driver validation, tail constraints, payload length, clipping/raising |
| REDEFINES | ✅ Fully Supported | `redefines_comprehensive.rs::test_redefines_shorter_overlay`, `redefines_comprehensive.rs::test_redefines_round_trip_preservation`, `comprehensive_redefines_odo_tests.rs::test_redefines_decode_all_views` | Shorter/equal/longer overlays, encode ambiguity, raw preservation |
| Level-88 (`level-88`) | ✅ Fully Supported | `golden_fixtures_ac2_level88_after_odo.rs::test_ac2_basic_level88_after_odo_pass`, `golden_fixtures_ac5_redefines_level88_interactions.rs::test_ac5_basic_level88_with_redefines_pass`, `test_level88_comma_support.rs` (10 tests) | Parse + codec with `FieldKind::Condition`, non-storage semantic validation |
| OCCURS (Fixed) | ✅ Fully Supported | `comprehensive_parser_tests.rs::test_occurs_fixed_arrays`, `comprehensive_redefines_odo_tests.rs::test_nested_fixed_occurs_allowed`, `redefines_comprehensive.rs::test_redefines_with_occurs` | Fixed-size array support with dedicated tests |
| SYNCHRONIZED | ✅ Fully Supported | `comprehensive_parser_tests.rs::test_synchronized_alignment` | Field alignment with padding calculation |
| BLANK WHEN ZERO | ✅ Fully Supported | `comprehensive_parser_tests.rs::test_blank_when_zero_parsing`, `comprehensive_numeric_tests.rs::test_blank_when_zero_comprehensive`, `decimal_edge_cases.rs::test_blank_when_zero_edge_cases` | Special value handling |
| Nested ODO / OCCURS (`nested-odo`) | ✅ O1-O4 Supported | See [Nested ODO Support Status](#nested-odo--occurs-behavior---support-status) for scenario breakdown | O1-O4✅ supported; O5/O6🚫 rejected by design; O7🚫 policy-limited by R4-R6 (`CBKS612_RENAME_ODO_NOT_SUPPORTED`) |
| RENAMES (`level-66-renames`) | ✅ Fully Supported (R1-R3) | `renames_codec_tests.rs::test_renames_r1_simple_decode`, `renames_codec_tests.rs::test_renames_r2_group_decode`, `schema_alias_lookup_tests.rs` (8 tests) | See [RENAMES Support Status](#renames-level-66---support-status) for scenario breakdown (R1-R3✅ with alias-aware lookup, R4-R6🚫 policy-limited) |
| Dialect Lever (`dialect`) | ✅ Fully Supported (D0-D4) | `dialect_d1_tests.rs` (27 tests), `dialect_cli_d2_tests.rs` (11 tests), `dialect_fixtures_d3_tests.rs` | ODO `min_count` interpretation: Normative (n), ZeroTolerant (0), OneTolerant (1) modes with CLI `--dialect` flag and `COPYBOOK_DIALECT` env var; D0 contract complete (commit a9609af) |

## Sign Handling

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| SIGN LEADING clause | ✅ Fully Supported | `copybook-core/tests/sign_separate_feature_enabled_tests.rs` | SIGN IS LEADING SEPARATE supported; default-enabled stable policy |
| SIGN TRAILING clause | ✅ Fully Supported | `copybook-core/tests/sign_separate_feature_enabled_tests.rs` | SIGN TRAILING SEPARATE supported; default-enabled stable policy |
| SIGN SEPARATE (`sign-separate`) | ✅ **Fully Supported** | `copybook-core/tests/sign_separate_feature_enabled_tests.rs`, `copybook-core/tests/schema_validation_edge_cases.rs`, `copybook-codec/tests/numeric_sign_separate_comprehensive.rs`, `copybook-codec/tests/sign_separate_golden_tests.rs`, `copybook-codec/tests/sign_separate_tests.rs` | Default-enabled; can be disabled explicitly via feature flag; encode + decode + round-trip |
| Overpunch (EBCDIC/ASCII) | ✅ Fully Supported | `decimal_edge_cases.rs::test_zoned_overpunch_by_codepage`, `decimal_edge_cases.rs::test_zoned_overpunch_comprehensive`, `comprehensive_numeric_tests.rs::test_zoned_decimal_ascii_sign_zones_comprehensive` | Comprehensive overpunch with EBCDIC zones |

## Record Formats

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| Fixed-Length | ✅ Fully Supported | `comprehensive_numeric_tests.rs::test_record_length_validation`, `binary_roundtrip_fidelity_tests.rs::test_customer_record_roundtrip_fidelity`, `enterprise_mainframe_production_scenarios.rs::test_enterprise_banking_transaction_processing` | Streaming support with fixed-length records |
| RDW (Variable-Length) | ✅ Fully Supported | `rdw_comprehensive.rs::test_rdw_normal_processing`, `comprehensive_rdw_tests.rs::test_rdw_basic_parsing`, `rdw_comprehensive.rs::test_rdw_length_recomputation_on_encode` | Record Descriptor Word with streaming iterator |

## Codepages (EBCDIC)

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| CP037 (US/Canada) | ✅ Fully Supported | Primary codepage across 35+ test files, `comprehensive_numeric_tests.rs::test_codepage_specific_behavior` | Default EBCDIC codepage |
| CP273 (German) | ✅ Fully Supported | `prop_codepage_parity_extra.rs`, `decimal_edge_cases.rs::test_zoned_overpunch_by_codepage` | Full character conversion support |
| CP500 (International) | ✅ Fully Supported | `prop_codepage_parity_extra.rs`, `decimal_edge_cases.rs::test_zoned_overpunch_by_codepage` | International variant support |
| CP1047 (Latinized) | ✅ Fully Supported | `prop_codepage_parity_extra.rs`, `decimal_edge_cases.rs::test_zoned_overpunch_by_codepage` | Latin-1 based EBCDIC |
| CP1140 (Euro) | ✅ Fully Supported | `prop_codepage_parity_extra.rs`, `decimal_edge_cases.rs::test_zoned_overpunch_by_codepage` | Euro currency support |
| ASCII (supplementary) | ✅ Fully Supported | `binary_roundtrip_fidelity_tests.rs::test_ascii_zoned_roundtrip_byte_identical`, `encode_options_zoned_encoding_tests.rs::test_encode_preserves_ascii_format` | For comparison/testing purposes |

## Edited PIC Clauses

| Feature | Status | Error Codes | Test Evidence |
|---------|--------|-------------|---------------|
| **E1: Parse + Schema** | ✅ Supported | - | `edited_pic_e1_tests.rs` (15 tests) |
| **E2: Decode (subset)** | ✅ Supported | CBKD421-423 | `edited_pic_decode_e2_tests.rs` (28 tests) |
| **E3.1: Basic Encode** | ✅ Supported | CBKE421-423 | `edited_pic_encode_e3_tests.rs` (672 lines) |
| **E3.2: Trailing Signs** | ✅ Supported | CBKE421-423 | `edited_pic_encode_e3_tests.rs` (E3.2 tests) |
| **E3.3-E3.6: Full Encode** | ✅ Supported | CBKE4xx | CR/DB, commas, asterisk, currency |
| Z (zero suppress) | ✅ E1/E2 | - | `test_e2_simple_z_editing_zzz9` |
| $ (currency) | ✅ E1/E2 | - | `test_e2_currency_dollar_zz_zzz_99` |
| +/- (sign) | ✅ E1/E2 | - | `test_e2_sign_editing_*` |
| CR/DB (credit/debit) | ✅ E1/E2 | - | `test_e2_sign_editing_trailing_cr_db` |
| * (check protect) | ✅ E1/E2 | - | `test_e2_check_protect_asterisk` |
| BLANK WHEN ZERO | ✅ E1/E2 | CBKD423 | `test_e2_blank_when_zero` |
| Complex patterns | ✅ E1/E2 | - | `test_e2_complex_patterns` (8 tests) |

**Phase Breakdown**:
- **E1 (Parse + Schema)**: ✅ Parses edited PICTURE clauses into `EditedNumeric` FieldKind with pattern metadata
- **E2 (Decode)**: ✅ Decodes EBCDIC/ASCII edited format to JSON numeric values (well-chosen subset)
- **E3.1 (Basic Encode)**: ✅ Basic numeric encoding with Z-editing, decimal point, leading sign
- **E3.2 (Trailing Signs)**: ✅ Trailing plus/minus sign encoding (+/-)
- **E3.3 (CR/DB)**: ✅ Credit/debit sign encoding (`test_e3_3_credit_positive`, `test_e3_3_debit_negative`)
- **E3.4 (Commas/Slashes)**: ✅ Insertion editing (`test_e3_4_comma_basic`, `test_e3_4_slash_date_format`)
- **E3.5 (Asterisk)**: ✅ Check-protect fill (`test_e3_5_asterisk_basic_star9`, `test_e3_5_asterisk_check_protect_example`)
- **E3.6 (Currency)**: ✅ Fixed/floating currency (`test_e3_6_currency_fixed`, `test_e3_6_currency_with_comma`)
- **E3.7 (Space B)**: 🚫 Not supported (`Space B` insertion remains unsupported in current parser/codec behavior)

**Well-Chosen Subset (E2)**:
- ZZZ9 (basic zero suppression)
- $ZZ,ZZZ.99 (currency with comma/decimal)
- +/-/CR/DB (sign editing)
- \*\*\*9 (check protection)
- BLANK WHEN ZERO clause

## Field Projection

| Feature | Status | Error Codes | Test Evidence |
|---------|--------|-------------|---------------|
| **Simple field selection** | ✅ Supported | CBKS703 | `projection_tests.rs::test_projection_simple_field_selection` |
| **Multiple field selection** | ✅ Supported | - | `projection_tests.rs::test_projection_multiple_fields` |
| **Group selection** | ✅ Supported | - | `projection_tests.rs::test_projection_group_selection_includes_all_children` |
| **ODO auto-counter** | ✅ Supported | CBKS701 | `projection_tests.rs::test_projection_odo_auto_includes_counter`, `projection_tests.rs::test_projection_cbks701_invalid_odo_missing_counter` |
| **RENAMES alias (R1-R3)** | ✅ Supported | CBKS702 | `projection_tests.rs::test_projection_renames_alias_expansion`, `projection_tests.rs::test_projection_cbks702_unresolved_alias` |
| **Level-88 with parent** | ✅ Supported | - | `projection_tests.rs::test_projection_level88_with_parent_field` |
| **CLI --select flag** | ✅ Supported | - | `cli_projection_integration.rs::test_cli_decode_with_select_simple_fields`, `cli_projection_integration.rs::test_cli_decode_with_select_comma_separated`, `cli_projection_integration.rs::test_cli_encode_with_projection` (7 tests) |
| **API project_schema()** | ✅ Supported | - | `projection_tests.rs::test_projection_fingerprint_updated`, `projection_tests.rs::test_projection_preserves_occurs_info` (19 tests total) |

**Projection Features**:
- **Comma-separated selection**: `--select "ID,NAME,BALANCE"`
- **Multiple flags**: `--select "ID" --select "NAME"`
- **Automatic ODO counter inclusion**: When selecting ODO arrays, counter fields auto-included
- **RENAMES alias resolution**: Aliases resolve to underlying storage fields (R1-R3)
- **Parent group preservation**: JSON structure maintains hierarchy
- **Error validation**: CBKS701 (invalid ODO), CBKS702 (unresolved alias), CBKS703 (field not found)

**CLI Integration**:
```bash
# Decode with field projection
copybook decode schema.cpy data.bin --output selected.jsonl \
  --format fixed --codepage cp037 --select "CUSTOMER-ID,BALANCE"

# Encode with field projection
copybook encode schema.cpy input.jsonl output.bin \
  --format fixed --codepage cp037 --select "CUSTOMER-ID,BALANCE"

# Verify with field projection
copybook verify schema.cpy data.bin \
  --format fixed --codepage cp037 --select "CUSTOMER-ID,BALANCE"
```

## Dialect Lever (ODO min_count Interpretation)

**Status**: ✅ **Fully Supported** (CLI + Library API)

**Evidence**:
- **Core Implementation**: `copybook_core::dialect` module with `Dialect` enum
- **Core Tests**: `dialect_d1_tests.rs` (27 tests, 581 lines) - comprehensive unit tests
- **CLI Tests**: `dialect_cli_d2_tests.rs` (11 tests, 275 lines) - CLI flag and env var integration
- **Fixture Tests**: `dialect_fixtures_d3_tests.rs` - golden fixtures for all three modes
- **Total Test Coverage**: 38+ tests across 3 test suites

### Dialect Modes

| Mode | CLI Flag | Description | Behavior | Use Case |
|------|----------|-------------|----------|----------|
| **Normative** | `--dialect n` | Strict enforcement | `min_count` enforced as declared | ANSI COBOL standard copybooks |
| **Zero-Tolerant** | `--dialect 0` | IBM Enterprise mode | `min_count` ignored (always 0) | IBM Enterprise COBOL copybooks |
| **One-Tolerant** | `--dialect 1` | Micro Focus mode | `min_count` clamped to 1 | Micro Focus COBOL copybooks |

### Configuration

**Precedence Order**:
1. CLI `--dialect` flag (highest priority)
2. `COPYBOOK_DIALECT` environment variable
3. Default value (`n` - Normative)

### CLI Integration

```bash
# Normative dialect (default) - min_count enforced as declared
copybook parse schema.cpy --dialect n

# Zero-tolerant dialect for IBM Enterprise COBOL
copybook decode schema.cpy data.bin --format fixed --codepage cp037 --dialect 0

# One-tolerant dialect for Micro Focus COBOL
copybook encode schema.cpy data.jsonl output.bin --format fixed --dialect 1

# Environment variable configuration
export COPYBOOK_DIALECT=0
copybook verify schema.cpy data.bin --format fixed

# CLI flag overrides environment variable
COPYBOOK_DIALECT=0 copybook parse schema.cpy --dialect 1  # Uses one-tolerant
```

### Library API

```rust
use copybook_core::{parse_copybook_with_options, ParseOptions};
use copybook_core::dialect::Dialect;

// Parse with specific dialect
let options = ParseOptions {
    dialect: Dialect::ZeroTolerant,  // IBM Enterprise mode
    ..ParseOptions::default()
};
let schema = parse_copybook_with_options(&copybook_text, &options)?;

// Available dialects
let normative = Dialect::Normative;       // Default: min_count enforced
let zero_tolerant = Dialect::ZeroTolerant;  // min_count ignored
let one_tolerant = Dialect::OneTolerant;   // min_count clamped to 1
```

### Behavior Examples

**Example 1: ODO with min_count > 0**
```cobol
       01  RECORD.
           05  COUNTER      PIC 9(3).
           05  ITEMS        OCCURS 1 TO 10 DEPENDING ON COUNTER
                            PIC X(10).
```

| Dialect | Behavior |
|---------|----------|
| `--dialect n` | Counter must be 1-10 (min_count=1 enforced) |
| `--dialect 0` | Counter can be 0-10 (min_count ignored) |
| `--dialect 1` | Counter must be 1-10 (min_count=1 enforced) |

**Example 2: ODO with min_count = 0**
```cobol
       01  RECORD.
           05  COUNTER      PIC 9(3).
           05  ITEMS        OCCURS 0 TO 10 DEPENDING ON COUNTER
                            PIC X(10).
```

| Dialect | Behavior |
|---------|----------|
| `--dialect n` | Counter can be 0-10 (min_count=0 allowed) |
| `--dialect 0` | Counter can be 0-10 (min_count=0 allowed) |
| `--dialect 1` | Counter must be 1-10 (min_count raised to 1) |

### Available Commands

The `--dialect` flag is supported on all copybook-processing commands:
- `parse` - Affects schema parsing and layout validation
- `inspect` - Affects layout display and field offset calculation
- `decode` - Affects ODO bounds checking during data decoding
- `encode` - Affects ODO bounds validation during encoding
- `verify` - Affects data validation against schema

### Design Documentation

See `docs/internal/features/d0_dialect_lever_contract.md` for complete specification, implementation phases (D0-D4), and design rationale.

## Determinism Validation

**Status**: ✅ **Fully Supported** (CLI + Library API)

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| **Decode determinism** | ✅ Supported | `determinism_cli.rs::determinism_decode_deterministic_exit_ok`, `determinism_cli.rs::determinism_decode_json_output_is_well_formed` | SHA-256 hash comparison across 2 decode passes |
| **Encode determinism** | ✅ Supported | `determinism_cli.rs::determinism_encode_deterministic_exit_ok` | SHA-256 hash comparison across 2 encode passes |
| **Round-trip determinism** | ✅ Supported | `determinism_cli.rs::determinism_round_trip_deterministic_exit_ok` | Decode-encode-decode cycle verification |
| **Human-readable output** | ✅ Supported | `determinism_cli.rs::determinism_decode_human_output_contains_verdict` | Shows DETERMINISTIC/NON-DETERMINISTIC verdict with round hashes |
| **JSON output (CI)** | ✅ Supported | `determinism_cli.rs::determinism_decode_json_output_is_well_formed` | Machine-parseable `is_deterministic` field |
| **Help/exit codes** | ✅ Supported | `determinism_cli.rs::determinism_help_shows_exit_codes` | Exit 0=deterministic, 2=non-deterministic, 3=error |

**CLI Integration**:
```bash
# Verify decode determinism
copybook determinism decode --format fixed --codepage cp037 schema.cpy data.bin

# Verify encode determinism
copybook determinism encode --format fixed --codepage cp037 schema.cpy input.jsonl

# Verify round-trip determinism
copybook determinism round-trip --format fixed --codepage cp037 schema.cpy data.bin

# JSON output for CI
copybook determinism decode --output json --format fixed --codepage cp037 schema.cpy data.bin
```

**Library API**: `copybook_codec::determinism` module provides programmatic access.

## Error Code Coverage

**Comprehensive error taxonomy with 48 error codes total** (Wave 1C audit: 42 with direct tests, 6 reserved/vestigial/internal):

### Error Code Audit Summary (Wave 1C)

| Category | Prefix | Total Codes | Directly Tested | Reserved/Vestigial |
|----------|--------|-------------|-----------------|-------------------|
| Parse Errors | CBKP* | 8 | 7 | 1 (CBKP051 - only Space `B` remains) |
| Schema Validation | CBKS* | 16 | 14 | 2 (internal) |
| Data Errors | CBKD* | 12 | 11 | 1 (vestigial) |
| Encode Errors | CBKE* | 8 | 7 | 1 (reserved) |
| Record Errors | CBKR* | 2 | 2 | 0 |
| Internal/Infra | CBKI*/CBKA* | 2 | 1 | 1 (internal) |
| **Total** | | **48** | **42** | **6** |

### Parse Errors (CBKP*)
- `CBKP001_SYNTAX`: Copybook syntax errors — `comprehensive_parser_tests.rs::test_error_context_with_line_numbers`
- `CBKP011_UNSUPPORTED_CLAUSE`: Unsupported COBOL clause — `comprehensive_parser_tests.rs::test_sign_clause_as_edited_pic_normative`
- `CBKP021_ODO_NOT_TAIL`: ODO not at tail — `golden_fixtures_ac4_sibling_after_odo_fail.rs::test_ac4_basic_storage_after_odo_fail`
- `CBKP022_NESTED_ODO`: Nested ODO rejected — `nested_odo_negative_tests.rs::test_o5_nested_odo_basic_rejection`
- `CBKP023_ODO_REDEFINES`: ODO over REDEFINES rejected — `nested_odo_negative_tests.rs::test_o6_odo_over_redefines_basic`
- `CBKP051_UNSUPPORTED_EDITED_PIC`: Unsupported edited PIC token (Space `B` insertion only; all other patterns supported)

### Schema Validation Errors (CBKS*)
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter not found — `odo_comprehensive.rs::test_odo_driver_in_redefines_rejection`
- `CBKS141_RECORD_TOO_LARGE`: Record size exceeds limit — `schema_validation_edge_cases.rs`
- `CBKS301_ODO_CLIPPED`: ODO count > max — `odo_comprehensive.rs::test_odo_strict_mode_clamp_fatal`
- `CBKS302_ODO_RAISED`: ODO count < min — `odo_comprehensive.rs::test_odo_lenient_mode_raise_to_minimum`
- `CBKS601-608`: RENAMES resolver errors — `renames_resolver_negative_tests.rs` (12 tests)
- `CBKS610-611`: R4-R6 rejection — `error_code_coverage_tests.rs::test_cbks610_multiple_redefines_with_r4r6_flag`
- `CBKS701_PROJECTION_INVALID_ODO`: Projection ODO error — `projection_tests.rs::test_projection_cbks701_invalid_odo_missing_counter`
- `CBKS702_PROJECTION_UNRESOLVED_ALIAS`: Projection alias error — `projection_tests.rs::test_projection_cbks702_unresolved_alias`
- `CBKS703_PROJECTION_FIELD_NOT_FOUND`: Projection field not found — `projection_tests.rs::test_projection_field_not_found_error`

### Data Errors (CBKD*)
- `CBKD101`: RENAMES without metadata — `error_code_tests.rs::test_cbkd101_renames_without_resolved_metadata`
- `CBKD421_EDITED_PIC_INVALID_FORMAT`: Edited PIC decode mismatch — `edited_pic_decode_e2_tests.rs`
- `CBKD422_EDITED_PIC_SIGN_MISMATCH`: Edited PIC sign error — `error_code_coverage_tests.rs::test_cbkd422_leading_plus_invalid_char`
- `CBKD423_EDITED_PIC_BLANK_WHEN_ZERO`: BLANK WHEN ZERO — `edited_pic_decode_e2_tests.rs`

### Encode Errors (CBKE*)
- `CBKE510`: Zoned/packed overflow — `error_code_tests.rs::test_cbke510_zoned_decimal_overflow`
- `CBKE515`: String length exceeds capacity — `error_code_tests.rs::test_cbke515_string_length_exceeds_capacity`
- `CBKE421_EDITED_PIC_ENCODE_INVALID_FORMAT`: Edited PIC encode mismatch — `edited_pic_encode_e3_tests.rs::test_e3_1_edge_invalid_character`
- `CBKE423_EDITED_PIC_ENCODE_OVERFLOW`: Edited PIC encode overflow — `edited_pic_encode_e3_tests.rs::test_e3_1_edge_overflow_value_too_long`

### Record/Infrastructure Errors
- `CBKI001`: Fixed reader configuration — `error_code_tests.rs::test_cbki001_fixed_reader_requires_lrecl`
- `CBKA001`: Baseline load error — `error_code_coverage_tests.rs::test_cbka001_baseline_load_missing_file`

See [ERROR_CODES.md](ERROR_CODES.md) for complete reference.

## Level-88 Condition Values - Support Status

**Status**: ✅ **Fully Supported** (parse + codec + structural validation)

**Evidence**:
- **Parse Support**: `FieldKind::Condition { values: Vec<String> }` in schema AST
- **Codec Support**: Non-storage semantic handling in decode/encode paths
- **Structural Validation**: Level-88 allowed after ODO (non-storage element)
- **VALUE Clause Syntax**: Both space-separated and comma-separated VALUE lists supported (ANSI COBOL-85 §4.1.2.4.3, IBM Enterprise COBOL) - Issue #86
- **Golden Fixtures**: 638 lines (`golden_fixtures_ac2_level88_after_odo.rs`) + 838 lines (`golden_fixtures_ac5_redefines_level88_interactions.rs`)
- **Test Results**: 6 passing tests in AC2, 8 passing tests in AC5, 10 additional comma syntax tests (`test_level88_comma_support.rs`)
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

## Nested ODO / OCCURS Behavior - Support Status

**Status**: ✅ **O1-O4 Fully Supported** | 🚫 **O5-O6 Rejected by Design**

Nested ODO support is split into explicit scenarios to track implementation decisions.
See `docs/design/NESTED_ODO_BEHAVIOR.md` (Issue #164) for complete design specification.

| ID  | Scenario                                | Status | Error Code            | Test Evidence                                                    |
|-----|-----------------------------------------|--------|-----------------------|------------------------------------------------------------------|
| O1  | Simple tail ODO                         | ✅     | -                     | `golden_fixtures_ac3_child_inside_odo.rs::test_ac3_basic_child_inside_odo_pass` |
| O2  | Tail ODO with DYNAMIC (AC1/AC2)         | ✅     | -                     | `odo_comprehensive.rs` (21 tests), `odo_counter_types.rs`        |
| O3  | Group-with-ODO tail (AC3)               | ✅     | -                     | `golden_fixtures_ac3_child_inside_odo.rs::test_ac3_nested_groups_inside_odo_pass` |
| O4  | ODO with sibling after (AC4)            | 🚫     | CBKP021_ODO_NOT_TAIL  | `golden_fixtures_ac4_sibling_after_odo_fail.rs` (8 negative tests)|
| O5  | Nested ODO (ODO inside ODO)             | 🚫     | CBKP022_NESTED_ODO    | Phase N1: reject; Phase N2: review if user demand emerges        |
| O6  | ODO over REDEFINES                      | 🚫     | CBKP023_ODO_REDEFINES | Phase N1: reject; Phase N3: dedicated design required            |
| O7  | ODO over RENAMES span (R4-R6 scenarios) | 🚫     | CBKS612_RENAME_ODO_NOT_SUPPORTED | Policy-limited by R4-R6 scope; parser rejects ODO spans |

**Evidence:**

- **O1 (Simple tail ODO)**:
  - **Parser + Codec**: Full support for tail ODO arrays with min/max bounds
  - **Test**: `golden_fixtures_ac3_child_inside_odo.rs::test_ac3_basic_child_inside_odo_pass` (89 lines)
  - **JSON shape**: Array of objects/scalars with runtime length determined by counter

- **O2 (Tail ODO with DYNAMIC)**:
  - **Parser + Codec**: Full support for `OCCURS 1 TO N DEPENDING ON` with runtime bounds
  - **Tests**: 21 tests in `odo_comprehensive.rs`, counter type tests in `odo_counter_types.rs`
  - **Runtime validation**: Clamping with `CBKS301_ODO_CLIPPED`/`CBKS302_ODO_RAISED` errors

- **O3 (Group-with-ODO tail)**:
  - **Parser + Codec**: Full support for nested groups inside ODO arrays
  - **Tests**: `golden_fixtures_ac3_child_inside_odo.rs` (5 tests: basic, nested, deep, enterprise, performance)
  - **JSON shape**: Array of nested objects with hierarchical structure preserved

- **O4 (ODO with sibling after)**:
  - **Parser**: Rejects storage siblings after ODO with `CBKP021_ODO_NOT_TAIL`
  - **Tests**: `golden_fixtures_ac4_sibling_after_odo_fail.rs` (8 comprehensive negative tests)
  - **Rationale**: Variable-length arrays cannot have fixed-offset siblings after them
  - **Exception**: Level-88 condition values (non-storage) permitted after ODO

- **O5 (Nested ODO)**:
  - **Decision**: 🚫 Rejected in Phase N1 due to complexity (schema nesting, counter scoping, memory layout)
  - **Error code**: `CBKP022_NESTED_ODO`
  - **Reconsideration**: Phase N2 if user demand emerges with concrete use cases

- **O6 (ODO over REDEFINES)**:
  - **Decision**: 🚫 Rejected in Phase N1 due to semantic conflict (fixed overlay vs variable length)
  - **Error code**: `CBKP023_ODO_REDEFINES`
  - **Future**: Phase N3 with dedicated REDEFINES + OCCURS design

- **O7 (ODO over RENAMES)**:
  - **Decision**: 🚫 Policy-limited by RENAMES R4-R6 scope; parser emits `CBKS612_RENAME_ODO_NOT_SUPPORTED`
  - **Reference**: See [RENAMES Support Status](#renames-level-66---support-status)

**Layout Impact**:
- **O1-O3**: ODO arrays compute variable offset ranges with runtime counter resolution
- **O4**: Parser-level rejection ensures no invalid layouts are created
- **O5-O6**: Not applicable (rejected before layout computation)

**Codec Integration**:
```rust
// O1-O3 example: Decode variable-length array
let counter_value = read_counter_field(&schema, data, counter_path)?;
let clamped_count = clamp_odo_count(counter_value, min, max)?;
for i in 0..clamped_count {
    let occurrence = decode_occurrence(&schema, data, occurrence_offset)?;
    array.push(occurrence);
}
```

**Error Codes:**
- **CBKP021_ODO_NOT_TAIL**: Fatal parser error (O4) - storage sibling after ODO
- **CBKS301_ODO_CLIPPED**: Runtime warning/error (O2) - counter > max
- **CBKS302_ODO_RAISED**: Runtime warning/error (O2) - counter < min
- **CBKP022_NESTED_ODO**: Fatal parser error (O5) - nested ODO rejected by design
- **CBKP023_ODO_REDEFINES**: Fatal parser error (O6) - ODO over REDEFINES rejected by design

**Known Limitations:**
- **Nested ODO** (O5): Not supported by design; pre-normalize on mainframe or use fixed OCCURS
- **ODO + REDEFINES** (O6): Not supported; use REDEFINES with fixed OCCURS instead
- **ODO + RENAMES** (O7): Rejected by policy in RENAMES R4-R6 scope (`CBKS612_RENAME_ODO_NOT_SUPPORTED`)

**Implementation Phases:**
- **Phase N1 (Current)**: Design contract + support matrix + negative error codes ✅
- **Phase N2 (Optional)**: Nested ODO support if user demand emerges
- **Phase N3 (Future)**: REDEFINES + OCCURS interactions with dedicated design

## RENAMES (Level-66) - Support Status

RENAMES support is split into explicit scenarios to track implementation progress.
See `docs/design/RENAMES_NESTED_GROUPS.md` for complete design specification.

| ID                          | Category                     | Status  | Notes                                                            |
|-----------------------------|------------------------------|---------|------------------------------------------------------------------|
| `renames-same-scope-field`  | 66-level – same-scope fields | ✅      | Parser + same-scope resolver; codec decode ✅; encode skipped (non-storage) |
| `renames-same-scope-group`  | 66-level – group alias       | ✅      | Parser + resolver (R2) with correct tree building; codec decode ✅ |
| `renames-nested-group`      | 66-level – nested group      | ✅      | Parser + resolver (R3) with recursive target lookup; codec decode ✅ |
| `renames-codec-projection`  | 66-level – codec projection  | ✅      | Schema provides `find_field_or_alias` and `resolve_alias_to_target` for codec integration |
| `renames-redefines`         | 66-level – over REDEFINES    | 🚫      | Out of scope; requires separate design for interaction semantics |
| `renames-occurs`            | 66-level – over OCCURS       | 🚫      | Out of scope; requires separate design for array aliasing        |
| `renames-odo`               | 66-level – over ODO          | 🚫      | Policy-limited; ODO in RENAMES span is not supported (`CBKS612_RENAME_ODO_NOT_SUPPORTED`) |

**Evidence:**

- `renames-same-scope-field`:
  - **Parser**: 11 tests (syntax, keyword variants, qualified names) in `renames_parser_tests.rs`
  - **Hierarchy**: 3 tests (level-66/88 placement) in `renames_hierarchy_tests.rs`
  - **Resolver positive**: 4 tests (THRU/THROUGH, QNAME) in `renames_resolver_positive_tests.rs`
  - **Resolver negative**: 12 tests (all error codes) in `renames_resolver_negative_tests.rs`
  - **Total**: 30 comprehensive tests across 4 test suites
- `renames-same-scope-group` (R2):
  - **Parser fix**: Level-66 tree building corrected (PR #162) - group children properly preserved
  - **Resolver**: Single-group detection via `FROM==THRU` + storage children check in `layout.rs`
  - **Tests**: `renames_r2_same_scope_group` test in `renames_resolver_positive_tests.rs`
  - **Schema API**: Alias lookup methods added (`find_field_or_alias`, `resolve_alias_to_target`)
- `renames-nested-group` (R3):
  - **Resolver**: Recursive nested lookup via `find_field_by_name` for targets not found as siblings
  - **Tests**: `renames_r3_nested_group` test in `renames_resolver_positive_tests.rs`
  - **Schema API**: Tests in `schema_alias_lookup_tests.rs` verify alias resolution to nested targets
- `renames-codec-projection`:
  - **Schema API**: `find_field_or_alias()` - finds field by path or RENAMES alias name
  - **Schema API**: `resolve_alias_to_target()` - resolves alias to first storage member
  - **Tests**: 8 tests in `schema_alias_lookup_tests.rs` covering R2/R3 alias lookup and resolution
  - **Codec Decode**: ✅ Implemented - aliases resolve to storage fields during decode
  - **Codec Encode**: ✅ Skipped by design - RENAMES are non-storage elements, encode uses storage fields directly
  - **Design**: Aliases resolve to storage fields; no duplicate JSON keys (architectural principle)

**API Integration:**

```rust
// Schema definition includes Level-66 RENAMES
pub enum FieldKind {
    Renames { from_field: String, thru_field: String },
    // ... other field types
}

// Example: Parse RENAMES with qualified names
// 66 CUSTOMER-HEADER RENAMES CUSTOMER-ID OF RECORD-A THRU CUSTOMER-NAME OF RECORD-A.
```

**Resolver Error Codes:**

- **CBKS601**: Unknown `from` field
- **CBKS602**: Unknown `thru` field
- **CBKS604**: Reversed range (from after thru in source order)
- **CBKS605/606**: Cross-group boundaries (from/thru have storage-bearing children)
- **CBKS607**: OCCURS boundary violation (array in RENAMES range)
- **CBKS608**: Qualified name resolution failure
- **CBKS603**: Defined but NOT enforced (REDEFINES overlaps valid; source-order only)

**Layout Impact:** Level-66 fields consume zero bytes, implementing COBOL non-storage semantics. Resolver computes `(offset, length)` and populates `resolved_renames.members` with storage-bearing child paths.

**Implementation Roadmap:**

- **Phase R1 (Complete)**: Design doc + scenarios table + support matrix split
- **Phase R2 (Next)**: Resolver-based nested group attach + codec projection
- **Phase R3 (Future)**: Documentation updates + CI validation

## Test Infrastructure Summary

**Total Coverage**: **1652 passing tests** across the workspace (0 failures, 0 ignored in standard run)

**Golden Fixtures Framework**:
- Golden fixture tests with SHA-256 verification across 7+ AC files
- Enterprise production scenarios covering banking, insurance, retail, manufacturing, healthcare
- Performance integration with automated regression detection
- Structural validation: ODO, Level-88, REDEFINES interactions comprehensively tested

**Test Categories**:
- Unit tests: Per-crate unit coverage across all 5 workspace crates
- Integration tests: 79-231 per crate with end-to-end CLI verification
- Property-based tests: 512+ cases for COMP-3 (proptest)
- Enterprise scenarios: `enterprise_mainframe_production_scenarios.rs` (5 tests)
- Determinism validation: `determinism_cli.rs` (6 tests)
- Panic elimination: Zero unsafe code, comprehensive error path coverage
- Error code coverage: 42/48 codes directly tested (6 reserved/vestigial/internal)

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

**2026-02-15**: v0.4.3 - Comprehensive Audit Update
- Updated all test evidence columns with specific test function names (not just counts)
- Added Determinism Validation section with 6 CLI tests
- Added Error Code Audit Summary table (48 codes total, 42 directly tested)
- Enhanced Field Projection section with 19 core tests + 7 CLI tests
- Updated Edited PIC status to ✅ Fully Supported (E1-E3 complete, including E3.3-E3.7)
- Updated total test count: 1652 passing tests across workspace
- Added specific test evidence for codepage, record format, and sign handling features

**2025-12-31**: E3.1/E3.2 Edited PIC Encoding + Dialect Lever (D0 Complete)
- ✅ E3.1 Edited PIC Encoding complete (commit 976ca0f) - basic numeric encoding with Z-editing, decimal point, leading sign
- ✅ E3.2 Edited PIC Encoding complete - trailing plus/minus sign encoding
- ✅ D0 Dialect Lever Contract complete (commit a9609af)
  - Core implementation with `Dialect` enum (27 tests, 581 lines)
  - CLI integration with `--dialect` flag and `COPYBOOK_DIALECT` env var (11 tests, 275 lines)
  - Documentation complete (CLI_REFERENCE.md, CLAUDE.md, COBOL_SUPPORT_MATRIX.md)
- Added E3.1/E3.2 encode error codes (CBKE421-423)
- Updated test counts: 626+ passing tests
- Full dialect lever support: Normative (n), ZeroTolerant (0), OneTolerant (1)
- RENAMES R1-R3 codec support: decode ✅, encode ✅ skipped by design (non-storage semantics)

**2025-10-22**: Initial release
- Comprehensive feature matrix with test evidence
- Resolved Level-88 support contradiction (now ✅ Fully Supported)
- Added test infrastructure summary with 664 test function inventory
- Documented error code coverage (23 discrete codes)
- Performance standards with CI enforcement details
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
