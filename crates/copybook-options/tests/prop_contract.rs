// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_options::{
    DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    ZonedEncodingFormat,
};
use proptest::prelude::*;

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

    #[test]
    fn decode_options_json_roundtrip_preserves_key_fields(
        strict_mode in any::<bool>(),
        emit_meta in any::<bool>(),
        emit_filler in any::<bool>(),
        threads in 1usize..128,
        max_errors in proptest::option::of(0u64..5000),
    ) {
        let options = DecodeOptions::new()
            .with_format(RecordFormat::RDW)
            .with_json_number_mode(JsonNumberMode::Native)
            .with_emit_raw(RawMode::RecordRDW)
            .with_emit_meta(emit_meta)
            .with_emit_filler(emit_filler)
            .with_strict_mode(strict_mode)
            .with_max_errors(max_errors)
            .with_threads(threads)
            .with_preserve_zoned_encoding(true)
            .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic)
            .with_float_format(FloatFormat::IbmHex);

        let encoded = serde_json::to_vec(&options).expect("serialize decode options");
        let decoded: DecodeOptions = serde_json::from_slice(&encoded).expect("deserialize decode options");

        prop_assert_eq!(decoded.format, RecordFormat::RDW);
        prop_assert_eq!(decoded.json_number_mode, JsonNumberMode::Native);
        prop_assert_eq!(decoded.emit_raw, RawMode::RecordRDW);
        prop_assert_eq!(decoded.emit_meta, emit_meta);
        prop_assert_eq!(decoded.emit_filler, emit_filler);
        prop_assert_eq!(decoded.strict_mode, strict_mode);
        prop_assert_eq!(decoded.max_errors, max_errors);
        prop_assert_eq!(decoded.threads, threads);
        prop_assert_eq!(decoded.preferred_zoned_encoding, ZonedEncodingFormat::Ebcdic);
        prop_assert_eq!(decoded.float_format, FloatFormat::IbmHex);
    }

    #[test]
    fn encode_options_json_roundtrip_preserves_key_fields(
        strict_mode in any::<bool>(),
        bwz_encode in any::<bool>(),
        use_raw in any::<bool>(),
        coerce_numbers in any::<bool>(),
        threads in 1usize..128,
        max_errors in proptest::option::of(0u64..5000),
        override_tag in 0u8..3,
    ) {
        let override_value = match override_tag {
            0 => None,
            1 => Some(ZonedEncodingFormat::Ascii),
            _ => Some(ZonedEncodingFormat::Ebcdic),
        };

        let options = EncodeOptions::new()
            .with_format(RecordFormat::RDW)
            .with_json_number_mode(JsonNumberMode::Native)
            .with_use_raw(use_raw)
            .with_bwz_encode(bwz_encode)
            .with_strict_mode(strict_mode)
            .with_max_errors(max_errors)
            .with_threads(threads)
            .with_coerce_numbers(coerce_numbers)
            .with_preferred_zoned_encoding(ZonedEncodingFormat::Ascii)
            .with_zoned_encoding_override(override_value)
            .with_float_format(FloatFormat::IbmHex);

        let encoded = serde_json::to_vec(&options).expect("serialize encode options");
        let decoded: EncodeOptions = serde_json::from_slice(&encoded).expect("deserialize encode options");

        prop_assert_eq!(decoded.format, RecordFormat::RDW);
        prop_assert_eq!(decoded.json_number_mode, JsonNumberMode::Native);
        prop_assert_eq!(decoded.use_raw, use_raw);
        prop_assert_eq!(decoded.bwz_encode, bwz_encode);
        prop_assert_eq!(decoded.strict_mode, strict_mode);
        prop_assert_eq!(decoded.max_errors, max_errors);
        prop_assert_eq!(decoded.threads, threads);
        prop_assert_eq!(decoded.coerce_numbers, coerce_numbers);
        prop_assert_eq!(decoded.preferred_zoned_encoding, ZonedEncodingFormat::Ascii);
        prop_assert_eq!(decoded.zoned_encoding_override, override_value);
        prop_assert_eq!(decoded.float_format, FloatFormat::IbmHex);
    }
}
