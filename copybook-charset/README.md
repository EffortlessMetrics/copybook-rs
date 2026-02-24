# copybook-charset

Character set conversion utilities for EBCDIC/ASCII encoding.

This crate provides:
- EBCDIC to UTF-8 conversion using static lookup tables for performance
- UTF-8 to EBCDIC conversion
- Support for multiple EBCDIC code pages (CP037, CP273, CP500, CP1047, CP1140)
- Codepage and character encoding types
- Zoned decimal sign handling

## Usage

```rust
use copybook_charset::{ebcdic_to_utf8, utf8_to_ebcdic, Codepage, UnmappablePolicy};

// Convert EBCDIC bytes to UTF-8 string
let ebcdic_data = [0xC1, 0xC2, 0xC3]; // "ABC" in CP037
let result = ebcdic_to_utf8(&ebcdic_data, Codepage::CP037, UnmappablePolicy::Error)?;
assert_eq!(result, "ABC");

// Convert UTF-8 string to EBCDIC bytes
let utf8_text = "Hello";
let ebcdic = utf8_to_ebcdic(utf8_text, Codepage::CP037)?;
```

## Supported Code Pages

- **ASCII**: Standard ASCII encoding
- **CP037**: EBCDIC US/Canada
- **CP273**: EBCDIC Germany/Austria
- **CP500**: EBCDIC International
- **CP1047**: EBCDIC Open Systems
- **CP1140**: EBCDIC US/Canada with Euro

## License

AGPL-3.0-or-later - see LICENSE file for details.
