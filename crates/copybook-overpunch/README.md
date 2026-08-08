# copybook-overpunch

Compatibility crate for zoned decimal overpunch encoding and decoding.

The implementation is now owned by `copybook-codec`, which provides one
overpunch contract for ASCII and EBCDIC codepages. This package forwards the
historical API for migration compatibility.

## Public API

- `ZeroSignPolicy`
- `encode_ebcdic_overpunch_zone`
- `decode_ebcdic_overpunch_zone`
- `encode_overpunch_byte`
- `decode_overpunch_byte`
- `is_valid_overpunch`
- `get_all_valid_overpunch_bytes`

New code should import `copybook_codec::numeric::overpunch`.
