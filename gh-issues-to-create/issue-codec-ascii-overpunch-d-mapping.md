# [Task]: Correct and Verify ASCII Overpunch Sign Mapping

**Issue Description**

In `copybook-codec/src/numeric.rs`, the ASCII overpunch character mappings for zoned decimals contain a likely data corruption bug. The `TODO` comment on the mapping for the character `'D'` hints at this, but the issue is more significant.

The implementation appears to attempt to follow the common EBCDIC-to-ASCII overpunch sign convention, where characters `A-I` represent `+1` to `+9` and `J-R` represent `-1` to `-9`. However, the mapping for negative numbers is incorrect.

According to the EBCDIC standard:
- The character for `-4` should be `'M'` (EBCDIC `D4`).
- The character for `-0` should be `'}'` (EBCDIC `D0`).

The current code incorrectly maps:
- `'M'` (ASCII `0x4D`) to `-0`.
- `'D'` (ASCII `0x44`) to `-4`.

The mapping for `'D'` appears to be a workaround for the incorrect `'M'` mapping, as `'D'` is not part of the standard `J-R` sequence for negative numbers. This will cause incorrect decoding of standard mainframe data.

**Files and Locations:**

- `copybook-codec/src/numeric.rs:701`
- `copybook-codec/src/numeric.rs:865`
- `copybook-codec/src/numeric.rs:1415`

**Code Context (Decoding):**

```rust
// copybook-codec/src/numeric.rs

                let (actual_digit, sign) = match byte {
                    0x30..=0x39 => ((byte - 0x30) as i64, false), // '0'-'9' positive
                    0x7B => (0, false),                           // '{' = +0
                    0x44 => (4, true), // 'D' = -4 (TODO: verify correct mapping)
                    0x41..=0x43 | 0x45..=0x49 => ((byte - 0x40) as i64, false), // 'A'-'C','E'-'I' positive (1-3,5-9)
                    0x7D => (3, false), // '}' = +3 (based on test)
                    0x4A => (1, true),  // 'J' = -1
                    0x4B => (2, true),  // 'K' = -2
                    0x4C => (3, true),  // 'L' = -3
                    0x4D => (0, true),  // 'M' = -0  <-- INCORRECT
                    0x4E => (5, true),  // 'N' = -5
                    0x4F => (6, true),  // 'O' = -6
                    0x50 => (7, true),  // 'P' = -7
                    0x51 => (8, true),  // 'Q' = -8
                    0x52 => (9, true),  // 'R' = -9
                    // ...
                };
```

**Proposed Fix**

The mappings should be corrected in all relevant `decode` and `encode` functions to align with the standard EBCDIC-based convention.

### Step 1: Correct the Decoder

Update the `match` statements in `decode_zoned_decimal` and `decode_zoned_decimal_with_encoding`.

```rust
// copybook-codec/src/numeric.rs

                let (actual_digit, sign) = match byte {
                    // Positive signs
                    0x7B => (0, false), // '{' = +0
                    0x41..=0x49 => ((byte - 0x40) as i64, false), // 'A'-'I' = +1 to +9

                    // Negative signs
                    0x7D => (0, true),  // '}' = -0
                    0x4A..=0x52 => ((byte - 0x49) as i64, true), // 'J'-'R' = -1 to -9

                    // Unsigned digits
                    0x30..=0x39 => ((byte - 0x30) as i64, false),

                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII overpunch character 0x{byte:02X}"),
                        ));
                    }
                };
```
*Note: A more compact range-based mapping is proposed for clarity and correctness. `0x4A` ('J') becomes `0x4A - 0x49 = 1`, and `0x52` ('R') becomes `0x52 - 0x49 = 9`. This correctly maps the `J-R` range. The mapping for `'M'` (0x4D) becomes `0x4D - 0x49 = 4`, which is correct for `-4`.* The mapping for `}` is also corrected to `-0`.

### Step 2: Correct the Encoder

The `encode_zoned_decimal` and `encode_zoned_decimal_with_format` functions must also be corrected.

```rust
// copybook-codec/src/numeric.rs

                let overpunch_byte = if decimal.negative {
                    // ASCII negative overpunch characters (J-R for 1-9, } for 0)
                    match digit {
                        0 => 0x7D, // '}' = -0
                        1..=9 => 0x49 + digit,
                        _ => unreachable!(),
                    }
                } else {
                    // ASCII positive overpunch characters (A-I for 1-9, { for 0)
                    match digit {
                        0 => 0x7B, // '{' = +0
                        1..=9 => 0x40 + digit,
                        _ => unreachable!(),
                    }
                };
```

### Step 3: Add Comprehensive Tests

A new test should be added to `copybook-codec/src/numeric.rs` to validate the full range of positive and negative overpunch characters, ensuring this bug is fixed and does not regress.

```rust
// copybook-codec/src/numeric.rs (in `mod tests`)

    #[test]
    fn test_ascii_overpunch_sign_mappings() {
        let codepage = Codepage::ASCII;

        // Test negative values (J-R for -1 to -9, } for -0)
        assert_eq!(decode_zoned_decimal(&[0x7D], 1, 0, true, codepage).unwrap().value, 0);
        assert!(decode_zoned_decimal(&[0x7D], 1, 0, true, codepage).unwrap().negative);
        assert_eq!(decode_zoned_decimal(&[0x4A], 1, 0, true, codepage).unwrap().value, 1);
        assert_eq!(decode_zoned_decimal(&[0x4D], 1, 0, true, codepage).unwrap().value, 4);
        assert_eq!(decode_zoned_decimal(&[0x52], 1, 0, true, codepage).unwrap().value, 9);

        // Test positive values (A-I for +1 to +9, { for +0)
        assert_eq!(decode_zoned_decimal(&[0x7B], 1, 0, true, codepage).unwrap().value, 0);
        assert!(!decode_zoned_decimal(&[0x7B], 1, 0, true, codepage).unwrap().negative);
        assert_eq!(decode_zoned_decimal(&[0x41], 1, 0, true, codepage).unwrap().value, 1);
        assert_eq!(decode_zoned_decimal(&[0x44], 1, 0, true, codepage).unwrap().value, 4);
        assert_eq!(decode_zoned_decimal(&[0x49], 1, 0, true, codepage).unwrap().value, 9);
    }
```