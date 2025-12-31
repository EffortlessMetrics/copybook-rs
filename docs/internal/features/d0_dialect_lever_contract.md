# D0 Dialect Lever Contract: OCCURS Fixed with DEPENDING ON Lower Bound

**Issue:** #51  
**Version:** v0.5.0  
**Status:** D0 - Config + CLI contract (docs-only PR)

---

## Contract Section

### Problem Statement

Different COBOL dialects have different behaviors for `OCCURS min TO max DEPENDING ON counter`:

- **IBM Enterprise COBOL**: Requires `min = 0` for ODO arrays
- **Micro Focus COBOL**: Allows `min > 0` for ODO arrays
- **Other dialects**: Varying behavior

This dialect lever allows users to configure which behavior to enforce during parsing.

### Default Behavior

The default mode is `"n"` (no enforcement), which maintains the current behavior of accepting any `min` value.

---

## Config Key Section

### Configuration Key

```toml
[parser]
occurs_fixed_with_depends_lower_bound = "n" | "0" | "1"
```

### CLI Flag

```
--dialect-occurs-fixed-with-depends-lower-bound <n|0|1>
```

### Environment Variable

```
COPYBOOK_DIALECT_OCCURS_FIXED_WITH_DEPENDS_LOWER_BOUND=n|0|1
```

### Priority Order

1. CLI flag (highest priority)
2. Environment variable
3. Config file
4. Default value (`"n"`) (lowest priority)

---

## Behavior Section

### Mode Definitions

| Value | Meaning | Behavior |
|-------|---------|----------|
| `"n"` | No enforcement (default) | Accepts `min TO max` with any `min` value (current behavior) |
| `"0"` | IBM Enterprise mode | Rejects `min > 0` with error `CBKP051_ODO_FIXED_LOWER_BOUND` |
| `"1"` | Micro Focus mode | Accepts `min TO max` with any `min` value (same as `"n"`) |

### New Error Code

| Code | Description | Severity (Strict) | Severity (Lenient) |
|-------|-------------|-------------------|----------------------|
| `CBKP051_ODO_FIXED_LOWER_BOUND` | ODO lower bound must be 0 in IBM Enterprise mode | Fatal | Fatal |

### Error Message Format

```
error: ODO lower bound must be 0 in IBM Enterprise dialect mode
  --> <copybook>:<line>:<col>
   |
<line> | <source line>
   |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = help: Change `OCCURS min TO max` to `OCCURS 0 TO max` or set dialect mode to "n" or "1"
```

---

## Worked Example Section

### Copybook Example

```cobol
       01  RECORD.
           05  COUNT-ITEM    PIC 9(3).
           05  ARRAY-ITEM     OCCURS 1 TO 10 DEPENDING ON COUNT-ITEM
                             PIC X(10).
```

### Behavior by Mode

| Mode | Result | Reason |
|------|--------|--------|
| `"n"` | Parse succeeds | Accepts `min = 1` (current behavior) |
| `"0"` | Parse fails with `CBKP051` | Rejects `min > 0` (IBM Enterprise mode) |
| `"1"` | Parse succeeds | Accepts `min = 1` (Micro Focus mode) |

### Example Error Output (Mode `"0"`)

```
error: ODO lower bound must be 0 in IBM Enterprise dialect mode
  --> example.cpy:3:28
   |
3  |            05  ARRAY-ITEM     OCCURS 1 TO 10 DEPENDING ON COUNT-ITEM
   |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = help: Change `OCCURS 1 TO 10` to `OCCURS 0 TO 10` or set dialect mode to "n" or "1"
```

### Additional Examples

#### Example 1: Valid in All Modes

```cobol
       01  RECORD.
           05  COUNT-ITEM    PIC 9(3).
           05  ARRAY-ITEM     OCCURS 0 TO 10 DEPENDING ON COUNT-ITEM
                             PIC X(10).
```

| Mode | Result |
|------|--------|
| `"n"` | Parse succeeds |
| `"0"` | Parse succeeds |
| `"1"` | Parse succeeds |

#### Example 2: Fixed OCCURS (Not Affected)

```cobol
       01  RECORD.
           05  ARRAY-ITEM     OCCURS 10 TIMES
                             PIC X(10).
```

| Mode | Result |
|------|--------|
| `"n"` | Parse succeeds |
| `"0"` | Parse succeeds |
| `"1"` | Parse succeeds |

---

## Implementation Plan Section

### D0: Config + CLI contract (docs-only PR) **← Current Phase**

- [x] Define config key, CLI flag, env var mapping
- [x] Document behavior for each mode
- [x] Provide worked example
- [x] Define new error code `CBKP051_ODO_FIXED_LOWER_BOUND`

**Deliverables:**
- This contract document

### D1: Core implementation (copybook-core)

- [ ] Add `occurs_fixed_with_depends_lower_bound` field to `ParseOptions`
  - Location: [`copybook-core/src/parser.rs:40-66`](copybook-core/src/parser.rs:40-66)
- [ ] Update `Default` implementation (default = `"n"`)
- [ ] Modify `parse_occurs_clause()` to validate lower bound
  - Location: [`copybook-core/src/parser.rs:956-1052`](copybook-core/src/parser.rs:956-1052)
- [ ] Add error code `CBKP051_ODO_FIXED_LOWER_BOUND` to error module
  - Location: [`copybook-core/src/error.rs`](copybook-core/src/error.rs)

**Implementation Details:**

```rust
// In ParseOptions struct
pub struct ParseOptions {
    pub emit_filler: bool,
    pub codepage: String,
    pub allow_inline_comments: bool,
    pub strict: bool,
    pub strict_comments: bool,
    pub occurs_fixed_with_depends_lower_bound: String,  // NEW
}

// In Default impl
impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            emit_filler: false,
            codepage: "cp037".to_string(),
            allow_inline_comments: true,
            strict: false,
            strict_comments: false,
            occurs_fixed_with_depends_lower_bound: "n".to_string(),  // NEW
        }
    }
}

// In parse_occurs_clause()
if let Occurs::ODO { min, max, counter_path } = &mut occurs {
    if options.occurs_fixed_with_depends_lower_bound == "0" && *min > 0 {
        return Err(ParseError::OdoFixedLowerBound {
            span: occurs_span,
            min: *min,
            max: *max,
        });
    }
}
```

### D2: CLI integration (copybook-cli)

- [ ] Add CLI flag to parse/decode/encode commands
  - Location: [`copybook-cli/src/main.rs:155-167`](copybook-cli/src/main.rs:155-167)
- [ ] Wire through `ParseOptionsConfig`
  - Location: [`copybook-cli/src/utils.rs:65-79`](copybook-cli/src/utils.rs:65-79)

**Implementation Details:**

```rust
// In CLI argument definition
.arg(Arg::new("dialect-occurs-fixed-with-depends-lower-bound")
    .long("dialect-occurs-fixed-with-depends-lower-bound")
    .value_name("n|0|1")
    .help("Dialect mode for OCCURS fixed with DEPENDING ON lower bound")
    .possible_values(["n", "0", "1"])
    .default_value("n"))

// In ParseOptionsConfig
impl ParseOptionsConfig {
    pub fn to_parse_options(&self) -> ParseOptions {
        ParseOptions {
            // ... existing fields ...
            occurs_fixed_with_depends_lower_bound: self.occurs_fixed_with_depends_lower_bound.clone(),
        }
    }
}
```

### D3: Golden fixtures (tests-only PR)

- [ ] Add fixtures for mode `"n"` showing acceptance of `min > 0`
- [ ] Add fixtures for mode `"0"` showing rejection of `min > 0`
- [ ] Add fixtures for mode `"1"` showing acceptance of `min > 0`
- [ ] Add fixtures for all modes showing acceptance of `min = 0`

**Fixture Locations:**
- `copybook-core/tests/golden_fixtures_odo.rs`
- `copybook-cli/tests/cli_golden_fixtures.rs`

### D4: Docs/examples

- [ ] Update ROADMAP with dialect lever
- [ ] Add examples to user documentation
- [ ] Update CHANGELOG with new feature

---

## References

### Related Code Locations

- **Parser Stage**: [`copybook-core/src/parser.rs:956-1052`](copybook-core/src/parser.rs:956-1052)
- **Layout Stage**: [`copybook-core/src/layout.rs:227-260`](copybook-core/src/layout.rs:227-260)
- **Runtime Validation**: [`copybook-codec/src/odo_redefines.rs:33-136`](copybook-codec/src/odo_redefines.rs:33-136)
- **ParseOptions**: [`copybook-core/src/parser.rs:40-66`](copybook-core/src/parser.rs:40-66)
- **CLI Options**: [`copybook-cli/src/main.rs:155-167`](copybook-cli/src/main.rs:155-167)
- **ParseOptionsConfig**: [`copybook-cli/src/utils.rs:65-79`](copybook-cli/src/utils.rs:65-79)

### Existing ODO Error Codes

| Code | Description | Severity (Strict) | Severity (Lenient) |
|-------|-------------|-------------------|----------------------|
| `CBKS301_ODO_CLIPPED` | ODO count exceeds maximum, clipped to limit | Fatal | Warning |
| `CBKS302_ODO_RAISED` | ODO count below minimum, raised to limit | Fatal | Warning |
| `CBKS121_COUNTER_NOT_FOUND` | ODO counter field not found | Fatal | Fatal |
| `CBKP021_ODO_NOT_TAIL` | ODO array not at tail position | Fatal | Fatal |
| `CBKP022_NESTED_ODO` | ODO inside another OCCURS/ODO | Fatal | Fatal |
| `CBKP023_ODO_REDEFINES` | ODO over REDEFINES | Fatal | Fatal |
| `CBKP051_ODO_FIXED_LOWER_BOUND` | ODO lower bound must be 0 in IBM Enterprise mode | Fatal | Fatal | **NEW**

---

## Appendix: COBOL Dialect Reference

### IBM Enterprise COBOL

From IBM Enterprise COBOL documentation:

> When specifying the OCCURS clause with the DEPENDING ON phrase, the lower bound must be 0. The syntax `OCCURS min TO max DEPENDING ON counter` is only valid when min is 0.

### Micro Focus COBOL

From Micro Focus COBOL documentation:

> The OCCURS clause with the DEPENDING ON phrase allows any lower bound value. The syntax `OCCURS min TO max DEPENDING ON counter` is valid for any non-negative min value.

### Other Dialects

- **GNU COBOL (GnuCOBOL)**: Similar to Micro Focus, allows any lower bound
- **Fujitsu COBOL**: Requires `min = 0` (similar to IBM)
- **Unisys COBOL**: Allows any lower bound (similar to Micro Focus)

---

**Document Version:** 1.0  
**Last Updated:** 2025-12-23  
**Author:** D0 Contract Phase
