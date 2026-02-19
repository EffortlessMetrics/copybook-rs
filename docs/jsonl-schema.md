# JSONL Schema (`copybook.v1`)

copybook-rs emits a versioned JSON envelope for each decoded record. Each line in the
`copybook decode` output conforms to the structure below.

## Envelope Keys

| Key | Type | Description |
| --- | ---- | ----------- |
| `schema` | string | Schema version identifier (`copybook.v1`). |
| `record_index` | integer | Zero-based record number in the decoded stream. |
| `codepage` | string | Decoder code page (e.g., `cp037`, `ascii`). |
| `fields` | object | Map of decoded field values. Nested groups are represented as objects. |
| `schema_fingerprint` | string | Optional SHA-256 fingerprint of the copybook (present when `--emit-meta`). |
| `offset` | integer | Optional byte offset in the source stream (present when `--emit-meta`). |
| `length` | integer | Optional decoded record length in bytes (present when `--emit-meta`). |
| `raw_b64` | string | Optional base64-encoded record bytes when `--emit-raw` is `record` or `record+rdw`. |
| `<field>_raw_b64` | string | Optional field-level payloads when `--emit-raw` is `field`. |
| `_encoding_metadata` | object | Optional zoned encoding metadata (present when `--preserve-zoned-encoding`). |
| `errors` | array | Optional structured error diagnostics for lenient decoding. |

> **Compatibility:** legacy keys (`__raw_b64`, `__schema_id`, etc.) remain available when using the
> library API to ease migration from pre-v1 tooling. The CLI emits the versioned envelope described
> here.

## Example Record

```json
{
  "schema": "copybook.v1",
  "record_index": 42,
  "codepage": "cp037",
  "fields": {
    "ACCOUNT": "00012345",
    "BALANCE": "1025.75",
    "STATUS": "A"
  },
  "schema_fingerprint": "4b0f5e64d9f6ec98f1a1d8e584c9d1e2f6879c16efac5aa1f77adbc2e7c8d2f5",
  "offset": 1280,
  "length": 80,
  "raw_b64": "AAECAwQFBgcICQoLDA0ODxA="
}
```

## Field Ordering

Fields within the `fields` object are emitted in **pre-order traversal** of the schema tree.
This means fields appear in the same order as their declarations in the COBOL copybook,
depth-first. Group fields create nested JSON objects whose children follow the same
ordering guarantee.

Given a copybook:

```cobol
01 RECORD.
   05 HEADER.
      10 ACCOUNT   PIC X(8).
      10 TYPE      PIC X(1).
   05 BALANCE      PIC 9(7)V99.
```

The `fields` object is always:

```json
{
  "HEADER": {
    "ACCOUNT": "00012345",
    "TYPE": "C"
  },
  "BALANCE": "1025.75"
}
```

This ordering is deterministic and stable across runs, thread counts, and platforms.
See [NORMATIVE_SPEC.md](NORMATIVE_SPEC.md) section 8.6 for the parallel determinism guarantee.

## Numeric Rendering

Numeric fields are rendered according to the `--json-number` mode (default: `lossless`).

### Lossless Mode (default)

All numeric values are rendered as **JSON strings** with fixed-scale representation.
This preserves the exact decimal value without floating-point precision loss.

| COBOL Type | PIC | Raw Value | JSON Output |
| ---------- | --- | --------- | ----------- |
| Zoned decimal | `PIC 9(5)V99` | 12345 | `"123.45"` |
| Packed decimal | `PIC S9(7)V99 COMP-3` | -1025.75 | `"-1025.75"` |
| Binary integer | `PIC 9(4) COMP` | 42 | `"42"` |
| Alphanumeric | `PIC X(5)` | HELLO | `"HELLO"` |

Rules:
- Scale > 0: render with exactly `scale` digits after the decimal point.
- Scale = 0: render as integer string without decimal point.
- Negative zero is normalized to `"0"` (see [NORMATIVE_SPEC.md](NORMATIVE_SPEC.md) section 1.2).

### Native Mode

Numeric values are rendered as **JSON numbers** where the value fits in an IEEE 754
double (53-bit integer mantissa) without precision loss. Values exceeding this range
fall back to string representation.

| COBOL Type | PIC | Raw Value | JSON Output |
| ---------- | --- | --------- | ----------- |
| Zoned decimal | `PIC 9(5)V99` | 12345 | `123.45` |
| Binary integer | `PIC 9(4) COMP` | 42 | `42` |
| Large binary | `PIC 9(18) COMP` | 9007199254740993 | `"9007199254740993"` |

The threshold for fallback to string is `|value| > 2^53`.

See [NORMATIVE_SPEC.md](NORMATIVE_SPEC.md) section 1.1 for the fixed-scale rendering rules.

## REDEFINES Handling

When a COBOL copybook uses REDEFINES, all alternative views of the same storage are
emitted in declaration order. Each view is decoded independently from the underlying
bytes.

```cobol
01 RECORD.
   05 FIELD-A     PIC X(10).
   05 FIELD-B     REDEFINES FIELD-A PIC 9(10).
```

Produces:

```json
{
  "FIELD-A": "0000012345",
  "FIELD-B": "12345"
}
```

Both the primary field and all redefining fields appear as siblings in the `fields`
object. The decoder does not choose between alternatives -- all are emitted.

For **encoding** (JSON to binary), REDEFINES follows precedence rules defined in
[NORMATIVE_SPEC.md](NORMATIVE_SPEC.md) section 2:
1. If exactly one view is non-null, encode from that view.
2. If all views are null, fill with zeros.
3. If multiple views are non-null, return error `CBKE501_JSON_TYPE_MISMATCH`.

## FILLER Naming Convention

COBOL FILLER fields (unnamed padding) are omitted from output by default. When
`--emit-filler` is specified, FILLER fields are included with synthetic names
derived from their byte offset in the record:

```
_filler_XXXXXXXX
```

where `XXXXXXXX` is a zero-padded 8-digit decimal byte offset.

Example for a FILLER at byte offset 42:

```json
{
  "CUSTOMER-ID": "00012345",
  "_filler_00000042": "    ",
  "BALANCE": "1025.75"
}
```

This naming convention is deterministic and guaranteed stable. The pattern
`^_filler_[0-9]+$` is reserved in the [JSON schema](../schemas/record-format.json).

## Duplicate Name Disambiguation

When sibling fields share the same name in the COBOL copybook, subsequent
occurrences receive a `__dup<N>` suffix in declaration order:

```cobol
01 RECORD.
   05 NAME PIC X(10).
   05 NAME PIC X(20).
   05 NAME PIC X(30).
```

Produces:

```json
{
  "NAME": "FIRST     ",
  "NAME__dup2": "SECOND              ",
  "NAME__dup3": "THIRD                         "
}
```

The first occurrence keeps the original name; the second gets `__dup2`, the third
`__dup3`, and so on. The pattern `^[A-Z][A-Z0-9_-]*(__dup[0-9]+)?$` is reserved in
the [JSON schema](../schemas/record-format.json).

## ODO Array Representation

COBOL `OCCURS` and `OCCURS DEPENDING ON` (ODO) fields are represented as JSON arrays.

### Fixed OCCURS

A field with `OCCURS N TIMES` produces an array of exactly N elements:

```cobol
01 RECORD.
   05 ITEMS OCCURS 3 TIMES PIC X(5).
```

```json
{
  "ITEMS": ["ITEM1", "ITEM2", "ITEM3"]
}
```

### OCCURS DEPENDING ON

For ODO arrays, the array length equals the current value of the counter field.
The counter is read from the record data at decode time.

```cobol
01 RECORD.
   05 ITEM-COUNT   PIC 9(3).
   05 ITEMS         OCCURS 0 TO 10 DEPENDING ON ITEM-COUNT
                    PIC X(5).
```

If `ITEM-COUNT` is 2:

```json
{
  "ITEM-COUNT": "2",
  "ITEMS": ["AAAA ", "BBBB "]
}
```

In **lenient mode** (default), out-of-bounds counters are clamped:
- Counter < min: clamped to min, warning `CBKS302_ODO_RAISED` emitted.
- Counter > max: clamped to max, warning `CBKS301_ODO_CLIPPED` emitted.

In **strict mode** (`--strict`), out-of-bounds counters cause fatal errors.

Group-level OCCURS produce arrays of objects:

```json
{
  "TRANSACTIONS": [
    {"DATE": "20240115", "AMOUNT": "100.00"},
    {"DATE": "20240116", "AMOUNT": "250.50"}
  ]
}
```

## Level-88 Exclusion

Level-88 condition names are **not emitted** in CLI JSON output. Level-88 fields
define condition values in COBOL but occupy no storage; they are metadata about
their parent field's valid values.

```cobol
01 RECORD.
   05 STATUS       PIC X(1).
      88 ACTIVE    VALUE "A".
      88 INACTIVE  VALUE "I".
```

Produces only the storage field:

```json
{
  "STATUS": "A"
}
```

The `ACTIVE` and `INACTIVE` conditions do not appear in the `fields` object.

> **Library API note:** When using the library API directly (`decode_record`), Level-88
> fields encountered during field traversal produce a structured string representation
> for API consistency, but the CLI filters these from output.

## RENAMES Handling

Level-66 RENAMES fields define aliases over contiguous byte ranges. They are decoded
as alphanumeric strings spanning the aliased storage region.

```cobol
01 RECORD.
   05 FIRST-NAME   PIC X(10).
   05 LAST-NAME    PIC X(15).
   66 FULL-NAME    RENAMES FIRST-NAME THRU LAST-NAME.
```

Produces:

```json
{
  "FIRST-NAME": "JOHN      ",
  "LAST-NAME": "DOE            ",
  "FULL-NAME": "JOHN      DOE            "
}
```

RENAMES fields are resolved using their `resolved_renames` metadata (offset and length)
and decoded as raw byte spans converted via the active codepage. They are always
rendered as strings regardless of the underlying field types.

For **encoding**, RENAMES fields are skipped -- the aliased storage fields handle the
actual byte writes. Including a RENAMES field in encode input has no effect.

When using `--select` for field projection, RENAMES aliases are resolved to their
underlying storage fields (R1-R3 scenarios). If the alias spans fields that are not
all selected, error `CBKS702_PROJECTION_UNRESOLVED_ALIAS` is returned.

## Encoding Metadata

When `--preserve-zoned-encoding` is enabled, the envelope includes an
`_encoding_metadata` object that records the detected encoding format for
zoned decimal fields. This enables round-trip fidelity when the source data
uses a non-default encoding (e.g., ASCII zoned decimals in an EBCDIC file).

```json
{
  "schema": "copybook.v1",
  "record_index": 0,
  "codepage": "cp037",
  "fields": {
    "AMOUNT": "123"
  },
  "_encoding_metadata": {
    "AMOUNT": "ascii"
  }
}
```

The metadata object maps field names (or field paths) to encoding format strings.
Supported values include `"ascii"` and `"ebcdic"`. During encoding, this metadata
is read from the JSON input to select the correct zoned decimal encoding format,
ensuring byte-identical round-trips.

The `_encoding_metadata` key is reserved in the [JSON schema](../schemas/record-format.json)
and is only present when the option is enabled and at least one field has non-default encoding.

## Error Diagnostics

When decoding in lenient mode (default), field-level errors that do not abort
processing are collected into an optional `errors` array in the envelope.

```json
{
  "schema": "copybook.v1",
  "record_index": 5,
  "codepage": "cp037",
  "fields": {
    "ACCOUNT": "00012345",
    "BALANCE": null
  },
  "errors": [
    {
      "code": "CBKD201_INVALID_PACKED_DECIMAL",
      "field": "BALANCE",
      "message": "Invalid packed decimal nibble at byte 3"
    }
  ]
}
```

Each error object contains:

| Key | Type | Description |
| --- | ---- | ----------- |
| `code` | string | Stable error code (e.g., `CBKD201_INVALID_PACKED_DECIMAL`). |
| `field` | string | Field name or path where the error occurred. |
| `message` | string | Human-readable description of the error. |

The `errors` array is absent when no errors occur (not an empty array). Error codes
are stable within major versions and follow the taxonomy described in the project's
error code documentation:
- `CBKP*`: Parse errors
- `CBKS*`: Schema validation errors
- `CBKD*`: Data/decode errors
- `CBKE*`: Encode errors
- `CBKR*`: Record format errors

## Stability Contract

The `copybook.v1` schema identifier represents a stable output contract.

### Guarantees

- **Additive only**: New envelope keys may be added in minor versions. Existing keys
  will not be removed or have their semantics changed within the `v1` contract.
- **Field ordering**: Pre-order traversal ordering of the `fields` object is guaranteed.
- **Numeric rendering**: The rules for lossless and native modes are fixed for `v1`.
- **Determinism**: Identical inputs produce byte-identical JSONL output regardless
  of `--threads` count (see [NORMATIVE_SPEC.md](NORMATIVE_SPEC.md) section 8.6).

### Required Keys

Every record envelope always contains these four keys:

```json
["schema", "record_index", "codepage", "fields"]
```

All other keys are optional and controlled by CLI flags or decoding conditions.

### Versioning Policy

- The `schema` field value (`copybook.v1`) is immutable for the lifetime of the v1 contract.
- Breaking changes to field semantics, removal of keys, or changes to numeric rendering
  rules require a new schema version (e.g., `copybook.v2`).
- Consumers should tolerate unknown keys to remain forward-compatible.

## JSON Schema

The formal JSON schema lives in [`schemas/record-format.json`](../schemas/record-format.json) and
matches the envelope described above. Update both the schema file and this document when adding
new keys or schema versions.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
