# copybook-zoned-format

Single-purpose crate for zoned-decimal format detection and representation.

## Purpose

This crate centralizes `ZonedEncodingFormat` so all clients share a consistent
definition for ASCII vs. EBCDIC zoned decimal zone-nibble detection.

## Public API

- `ZonedEncodingFormat`
- `ZonedEncodingFormat::detect_from_byte`
- `ZonedEncodingFormat::is_ascii`
- `ZonedEncodingFormat::is_ebcdic`
- `ZonedEncodingFormat::is_auto`
- `ZonedEncodingFormat::description`
