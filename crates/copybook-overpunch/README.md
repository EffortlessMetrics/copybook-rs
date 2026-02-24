# copybook-overpunch

Zoned decimal overpunch encoding and decoding primitives for copybook-rs.

This crate isolates one responsibility:
last-digit overpunch mapping for ASCII and EBCDIC codepages, including
sign policy handling and validation helpers.

## Public API

- `ZeroSignPolicy`
- `encode_ebcdic_overpunch_zone`
- `decode_ebcdic_overpunch_zone`
- `encode_overpunch_byte`
- `decode_overpunch_byte`
- `is_valid_overpunch`
- `get_all_valid_overpunch_bytes`
