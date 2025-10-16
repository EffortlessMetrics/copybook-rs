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

## JSON Schema

The formal JSON schema lives in [`schemas/record-format.json`](../schemas/record-format.json) and
matches the envelope described above. Update both the schema file and this document when adding
new keys or schema versions.
