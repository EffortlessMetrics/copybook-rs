#![allow(clippy::expect_used)]

use copybook_codec::options::Codepage;
use copybook_codec::zoned_overpunch::{
    ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte,
};
use proptest::prelude::*;

const CODEPAGES: &[Codepage] = &[
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

proptest! {
    // EBCDIC codepages with preferred-zero policy should round-trip digits; zero may normalize sign.
    #[test]
    fn prop_ebcdic_overpunch_parity_extra(d in 0u8..=9, neg in any::<bool>()) {
        for &codepage in CODEPAGES {
            let encoded = encode_overpunch_byte(d, neg, codepage, ZeroSignPolicy::Preferred)
                .expect("encode");
            let (round_digit, round_neg) = decode_overpunch_byte(encoded, codepage)
                .expect("decode");

            prop_assert_eq!(round_digit, d);
            if d == 0 {
                prop_assert!(round_neg == neg || !round_neg);
            } else {
                prop_assert_eq!(round_neg, neg);
            }
        }
    }
}
