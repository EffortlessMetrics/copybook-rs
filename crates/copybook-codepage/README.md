# copybook-codepage

Compatibility forwarding package for the codepage domain types in
`copybook-charset`.

The canonical package provides:
- `Codepage` enum for ASCII and supported EBCDIC variants
- `UnmappablePolicy` enum for decode/encode fallback behavior
- `space_byte` for codepage-specific space byte selection
- `get_zoned_sign_table` for zoned decimal sign semantics

## Usage

```rust
use copybook_charset::{Codepage, UnmappablePolicy, get_zoned_sign_table, space_byte};

assert_eq!(space_byte(Codepage::ASCII), 0x20);
assert_eq!(space_byte(Codepage::CP037), 0x40);

let table = get_zoned_sign_table(Codepage::CP037);
assert_eq!(table[0xC], (true, false));

let policy = "replace".parse::<UnmappablePolicy>().unwrap();
assert_eq!(policy, UnmappablePolicy::Replace);
```

## License

AGPL-3.0-or-later - see LICENSE file for details.
