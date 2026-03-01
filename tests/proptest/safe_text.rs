// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_safe_text::{parse_isize, parse_usize, safe_parse_u16, safe_string_char_at};
use proptest::prelude::*;

use super::config::*;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_parse_usize_round_trip(value in 0u64..1_000_000u64) {
        let text = value.to_string();
        let parsed = parse_usize(&text, "prop").expect("roundtrip parse usize");
        prop_assert_eq!(parsed, value as usize);
    }

    #[test]
    fn prop_parse_isize_round_trip(value in -1_000_000isize..1_000_000isize) {
        let text = value.to_string();
        let parsed = parse_isize(&text, "prop").expect("roundtrip parse isize");
        prop_assert_eq!(parsed, value);
    }

    #[test]
    fn prop_parse_u16_round_trip(value in 0u16..=u16::MAX) {
        let text = value.to_string();
        let parsed = safe_parse_u16(&text, "prop").expect("roundtrip parse u16");
        prop_assert_eq!(parsed, value);
    }

    #[test]
    fn prop_safe_string_char_at_round_trip(
        text in prop::collection::vec(32u8..127u8, 1..64),
        index in 0usize..80usize,
    ) {
        let text = String::from_utf8(text).expect("ascii input");
        let max_index = text.len();
        let index = index % (max_index + 1);

        let result = safe_string_char_at(&text, index, "prop");

        if index < max_index {
            let expected = text.chars().nth(index).expect("char exists");
            prop_assert_eq!(result.expect("valid index"), expected);
        } else {
            prop_assert!(result.is_err());
        }
    }
}
