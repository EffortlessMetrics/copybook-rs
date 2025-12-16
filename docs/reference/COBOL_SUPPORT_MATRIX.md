# COBOL Feature Support Matrix

**Last Updated**: 2025-12-02
**Version**: copybook-rs v0.4.0
**Canonical Reference**: This document is the authoritative source for COBOL feature support

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
| DISPLAY (PIC X) | ✅ Fully Supported | `zoned_encoding_format_tests.rs` (21 tests), `comprehensive_numeric_tests.rs` (15 tests) | Alphanumeric fields with EBCDIC/ASCII conversion |
| Zoned Decimal (PIC 9) | ✅ Fully Supported | `zoned_encoding_format_tests.rs` (11 tests), `comprehensive_numeric_tests.rs`, `cobol_fixture_zoned_encoding_tests.rs` (7 tests) | EBCDIC zones and ASCII overpunch with proper sign handling |
| COMP-3 (Packed Decimal) | ✅ Fully Supported | `comp3_property_tests.rs` (512+ property cases), `comp3_format_verification.rs`, `decimal_edge_cases.rs` (9 tests) | Nibble sign processing, edge cases, overflow/underflow |
| BINARY (COMP) | ✅ Fully Supported | `comprehensive_numeric_tests.rs`, `binary_roundtrip_fidelity_tests.rs` (11 tests) | Various widths: 1/2/4/8 bytes, signed/unsigned |
| COMP-1/COMP-2 (`comp-1-comp-2`) | ❌ Not Supported | N/A | Single/double float - by design, not implemented |
| Edited PIC (`edited-pic`) | ⚠️ **Partially Supported (E1/E2)** | `edited_pic_e1_tests.rs` (15 tests), `edited_pic_decode_e2_tests.rs` (28 tests) | **E1**: Parse ✅ **E2**: Decode ✅ **E3**: Encode ⏳ v0.5.0 (see Edited PIC section below) |

## Structural Features

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| ODO (`occurs-depending`) | ✅ Fully Supported | `odo_comprehensive.rs` (21 tests), `golden_fixtures_odo.rs` (201 lines), `odo_tail_validation.rs` | Driver validation, tail constraints, payload length, clipping/raising |
| REDEFINES | ✅ Fully Supported | `redefines_comprehensive.rs` (18 tests, 20.5K), `comprehensive_redefines_odo_tests.rs` (16 tests) | Shorter/equal/longer overlays, encode ambiguity, raw preservation |
| Level-88 (`level-88`) | ✅ Fully Supported | `golden_fixtures_ac2_level88_after_odo.rs` (6 tests, 638 lines), `golden_fixtures_ac5_redefines_level88_interactions.rs` (8 tests, 838 lines) | Parse + codec with `FieldKind::Condition`, non-storage semantic validation |
| OCCURS (Fixed) | ✅ Fully Supported | Multiple test files | Fixed-size array support with 5+ dedicated tests |
| SYNCHRONIZED | ✅ Fully Supported | `comprehensive_parser_tests.rs` (22 tests) | Field alignment with padding calculation |
| BLANK WHEN ZERO | ✅ Fully Supported | Codec tests | 2+ tests for special value handling |
| Nested ODO / OCCURS (`nested-odo`) | ✅ O1-O4 Supported | See [Nested ODO Support Status](#nested-odo--occurs-behavior---support-status) for scenario breakdown | O1-O4✅ supported; O5-O6🚫 rejected by design; see Issue #164 |
| RENAMES (`level-66-renames`) | ✅ Fully Supported (R1-R3) | 30+ tests across 5 test suites (parser, hierarchy, resolver, schema API) | See [RENAMES Support Status](#renames-level-66---support-status) for scenario breakdown (R1-R3✅ with alias-aware lookup, R4-R6🚫 out of scope) |

## Sign Handling

| Feature | Status | Test Evidence | Notes |
|---------|--------|---------------|-------|
| SIGN LEADING clause | ❌ Not Supported | N/A | SIGN clause rejected; use signed zoned PIC without the SIGN keyword |
| SIGN TRAILING clause | ❌ Not Supported | N/A | SIGN clause rejected; use signed zoned PIC without the SIGN keyword |
| SIGN SEPARATE (`sign-separate`) | ❌ Not Supported | N/A | See Issue #44 for planned implementation |
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

## Edited PIC Clauses

| Feature | Status | Error Codes | Test Evidence |
|---------|--------|-------------|---------------|
| **E1: Parse + Schema** | ✅ Supported | - | `edited_pic_e1_tests.rs` (15 tests) |
| **E2: Decode (subset)** | ✅ Supported | CBKD421-423 | `edited_pic_decode_e2_tests.rs` (28 tests) |
| **E3: Encode** | 🔄 Planned v0.5.0 | CBKE4xx | - |
| Z (zero suppress) | ✅ E1/E2 | - | `test_e2_simple_z_editing_zzz9` |
| $ (currency) | ✅ E1/E2 | - | `test_e2_currency_dollar_zz_zzz_99` |
| +/- (sign) | ✅ E1/E2 | - | `test_e2_sign_editing_*` |
| CR/DB (credit/debit) | ✅ E1/E2 | - | `test_e2_sign_editing_trailing_cr_db` |
| * (check protect) | ✅ E1/E2 | - | `test_e2_check_protect_asterisk` |
| BLANK WHEN ZERO | ✅ E1/E2 | CBKD423 | `test_e2_blank_when_zero` |
| Complex patterns | ✅ E1/E2 | - | `test_e2_complex_patterns` (8 tests) |

**Phase Breakdown**:
- **E1 (Parse + Schema)**: Parses edited PICTURE clauses into `EditedNumeric` FieldKind with pattern metadata
- **E2 (Decode)**: Decodes EBCDIC/ASCII edited format to JSON numeric values (well-chosen subset)
- **E3 (Encode)**: Planned for v0.5.0 - will encode JSON numeric values to edited format

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
| **Group selection** | ✅ Supported | - | `projection_tests.rs::test_projection_group_includes_children` |
| **ODO auto-counter** | ✅ Supported | CBKS701 | `projection_tests.rs::test_projection_with_odo_auto_counter` |
| **RENAMES alias (R1-R3)** | ✅ Supported | CBKS702 | `projection_tests.rs::test_projection_with_renames_*` |
| **CLI --select flag** | ✅ Supported | - | `cli_projection_integration.rs` (6 tests) |
| **API project_schema()** | ✅ Supported | - | `copybook_core::projection` module |

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

## Error Code Coverage

Comprehensive error taxonomy with **29 discrete codes** tested across **664+ test functions**:

### Parse Errors (CBKP*)
- `CBKP001_SYNTAX`: Copybook syntax errors
- `CBKP011_UNSUPPORTED_CLAUSE`: Unsupported COBOL clause or feature
- `CBKP021_ODO_NOT_TAIL`: ODO array not at tail position
- `CBKP051_UNSUPPORTED_EDITED_PIC`: Edited PIC clauses (legacy v0.3.1-; now supported in v0.4.0+)
- Plus 4+ additional parse error codes

### Schema Validation Errors (CBKS*)
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field not found
- `CBKS141_RECORD_TOO_LARGE`: Record size exceeds maximum limit
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement (count > max)
- `CBKS302_ODO_RAISED`: ODO minimum value validation (count < min)
- `CBKS701_PROJECTION_INVALID_ODO`: Field projection with invalid ODO counter access
- `CBKS702_PROJECTION_UNRESOLVED_ALIAS`: Field projection with unresolved RENAMES alias
- `CBKS703_PROJECTION_FIELD_NOT_FOUND`: Field projection with non-existent field

### Data Errors (CBKD*)
- `CBKD*`: 15+ codes for invalid decimals, truncated records, character conversion, numeric overflow
- `CBKD421_EDITED_PIC_INVALID_FORMAT`: Edited PIC decode format mismatch (Phase E2)
- `CBKD422_EDITED_PIC_SIGN_MISMATCH`: Edited PIC decode sign error (Phase E2)
- `CBKD423_EDITED_PIC_BLANK_WHEN_ZERO`: Edited PIC BLANK WHEN ZERO handling (Phase E2)

### Encode Errors (CBKE*)
- `CBKE*`: 3+ codes for type mismatches, bounds violations, encoding failures
- `CBKE4xx`: Reserved for Edited PIC encode errors (Phase E3, planned v0.5.0)

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
| O7  | ODO over RENAMES span (R4/R5 scenarios) | 🚫     | Out of scope          | RENAMES R4-R6 explicitly deferred (see RENAMES section)          |

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
  - **Error code**: `CBKP022_NESTED_ODO` (to be added)
  - **Reconsideration**: Phase N2 if user demand emerges with concrete use cases

- **O6 (ODO over REDEFINES)**:
  - **Decision**: 🚫 Rejected in Phase N1 due to semantic conflict (fixed overlay vs variable length)
  - **Error code**: `CBKP023_ODO_REDEFINES` (to be added)
  - **Future**: Phase N3 with dedicated REDEFINES + OCCURS design

- **O7 (ODO over RENAMES)**:
  - **Decision**: 🚫 Out of scope per RENAMES R4-R6 policy
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
- **CBKP022_NESTED_ODO**: To be added in Phase N1 (O5)
- **CBKP023_ODO_REDEFINES**: To be added in Phase N1 (O6)

**Known Limitations:**
- **Nested ODO** (O5): Not supported by design; pre-normalize on mainframe or use fixed OCCURS
- **ODO + REDEFINES** (O6): Not supported; use REDEFINES with fixed OCCURS instead
- **ODO + RENAMES** (O7): Out of scope per RENAMES R4-R6 policy

**Implementation Phases:**
- **Phase N1 (Current)**: Design contract + support matrix + negative error codes
- **Phase N2 (Optional)**: Nested ODO support if user demand emerges
- **Phase N3 (Future)**: REDEFINES + OCCURS interactions with dedicated design

## RENAMES (Level-66) - Support Status

RENAMES support is split into explicit scenarios to track implementation progress.
See `docs/design/RENAMES_NESTED_GROUPS.md` for complete design specification.

| ID                          | Category                     | Status  | Notes                                                            |
|-----------------------------|------------------------------|---------|------------------------------------------------------------------|
| `renames-same-scope-field`  | 66-level – same-scope fields | ✅      | Parser + same-scope resolver; no separate JSON alias keys        |
| `renames-same-scope-group`  | 66-level – group alias       | ✅      | Parser + resolver (R2) with correct tree building; alias-aware lookup |
| `renames-nested-group`      | 66-level – nested group      | ✅      | Parser + resolver (R3) with recursive target lookup; alias-aware schema methods |
| `renames-codec-projection`  | 66-level – codec projection  | ✅      | Schema provides `find_field_or_alias` and `resolve_alias_to_target` for codec integration |
| `renames-redefines`         | 66-level – over REDEFINES    | 🚫      | Out of scope; requires separate design for interaction semantics |
| `renames-occurs`            | 66-level – over OCCURS       | 🚫      | Out of scope; requires separate design for array aliasing        |

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
