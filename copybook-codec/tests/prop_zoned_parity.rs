use copybook_codec::options::Codepage;
use copybook_codec::zoned_overpunch::{
    ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte,
};
use proptest::prelude::*;

proptest! {
    // ASCII parity: overpunch byte must round-trip digit and sign without normalization.
    #[test]
    fn prop_ascii_overpunch_parity(d in 0u8..=9, neg in any::<bool>()) {
        let byte = encode_overpunch_byte(d, neg, Codepage::ASCII, ZeroSignPolicy::Positive)
            .expect("encode ascii");
        let (round_digit, round_neg) = decode_overpunch_byte(byte, Codepage::ASCII)
            .expect("decode ascii");
        prop_assert_eq!(round_digit, d);
        prop_assert_eq!(round_neg, neg);
    }

    // CP037 parity: preferred-zero policy normalizes zero sign nibble while preserving non-zero signs.
    #[test]
    fn prop_cp037_overpunch_parity(d in 0u8..=9, neg in any::<bool>()) {
        let byte = encode_overpunch_byte(d, neg, Codepage::CP037, ZeroSignPolicy::Preferred)
            .expect("encode cp037");
        let (round_digit, round_neg) = decode_overpunch_byte(byte, Codepage::CP037)
            .expect("decode cp037");

        prop_assert_eq!(round_digit, d);
        if d == 0 {
            prop_assert!(round_neg == neg || !round_neg);
        } else {
            prop_assert_eq!(round_neg, neg);
        }
    }
}
