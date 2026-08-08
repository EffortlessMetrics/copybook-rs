# copybook-zoned-format

Compatibility crate for zoned-decimal format detection and representation.

## Purpose

The implementation is now owned by `copybook-codec`, so codec consumers share
one definition for ASCII vs. EBCDIC zoned decimal zone-nibble detection. This
package forwards the historical API for migration compatibility.

## Public API

- `ZonedEncodingFormat`
- `ZonedEncodingFormat::detect_from_byte`
- `ZonedEncodingFormat::is_ascii`
- `ZonedEncodingFormat::is_ebcdic`
- `ZonedEncodingFormat::is_auto`
- `ZonedEncodingFormat::description`

New code should import `copybook_codec::numeric::zoned::ZonedEncodingFormat`.
