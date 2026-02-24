# copybook-options

Configuration primitives for copybook codec behavior.

This crate has one responsibility: define and validate encode/decode option
contracts (record format, numeric mode, raw capture mode, zoned encoding
preferences, and float format), while re-exporting charset policy types used by
those contracts.

## Public API

- `DecodeOptions`
- `EncodeOptions`
- `RecordFormat`
- `JsonNumberMode`
- `RawMode`
- `ZonedEncodingFormat`
- `FloatFormat`
- `Codepage`
- `UnmappablePolicy`
