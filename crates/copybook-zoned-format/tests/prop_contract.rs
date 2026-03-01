// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use proptest::prelude::*;

use copybook_zoned_format::ZonedEncodingFormat;

proptest! {
    #[test]
    fn detect_from_byte_matches_zone_nibble(byte in any::<u8>()) {
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        let expected = match byte >> 4 {
            0x3 => Some(ZonedEncodingFormat::Ascii),
            0xF => Some(ZonedEncodingFormat::Ebcdic),
            _ => None,
        };
        prop_assert_eq!(detected, expected);
    }
}
