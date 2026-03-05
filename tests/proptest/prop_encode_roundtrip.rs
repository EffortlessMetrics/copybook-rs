// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for **encode → decode** roundtrip fidelity.
//!
//! Each test generates random valid JSON (or numeric values), encodes it into
//! the COBOL binary representation, then decodes it back and asserts that the
//! result matches the original.  This is the dual of the existing decode→encode
//! tests in `prop_codec_roundtrip` and `prop_field_roundtrip`.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::panic::catch_unwind;

use copybook_codec::numeric::{
    decode_binary_int, decode_float_double, decode_float_single, decode_packed_decimal,
    decode_zoned_decimal, decode_zoned_decimal_sign_separate, encode_binary_int,
    encode_float_double, encode_float_single, encode_packed_decimal, encode_zoned_decimal,
    encode_zoned_decimal_sign_separate,
};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use copybook_core::{SignPlacement, SignSeparateInfo};
use proptest::prelude::*;

use super::config::{DEFAULT_CASES, QUICK_CASES};

// ============================================================================
// 1. DISPLAY field: random valid JSON string → encode → decode → compare
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Random PIC 9 value as JSON → encode → decode → same numeric value.
    #[test]
    fn prop_encode_rt_display_numeric(
        digits in 1u16..=9,
        raw_value in 0u64..=999_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let value = raw_value % max_val;
        let value_str = value.to_string();

        let copybook = format!(
            "       01 REC.\n           05 FLD PIC 9({digits})."
        );
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": value_str } });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse::<i64>().ok())))
            .expect("numeric value");
        prop_assert_eq!(out_val, i64::try_from(value).unwrap());
    }
}

// ============================================================================
// 2. COMP-3: random packed decimal → encode → decode → compare
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Signed COMP-3: value string → encode → decode → re-encode → bytes match.
    #[test]
    fn prop_encode_rt_comp3(
        digits in 1u16..=9,
        scale in 0i16..=3,
        raw_value in 0u64..=999_999_999u64,
        negative in any::<bool>(),
    ) {
        #[allow(clippy::cast_possible_wrap)]
        let scale = scale.min((digits as i16) - 1).max(0);
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;
        let sign = if negative && abs_val != 0 { "-" } else { "" };

        let value_str = if scale > 0 {
            let width = usize::from(digits);
            let s = format!("{abs_val:0>width$}");
            let sc = usize::try_from(scale).unwrap_or(0);
            let split = s.len().saturating_sub(sc);
            let (int, frac) = s.split_at(split);
            let int = if int.is_empty() { "0" } else { int };
            format!("{sign}{int}.{frac}")
        } else {
            format!("{sign}{abs_val}")
        };

        let encoded = encode_packed_decimal(&value_str, digits, scale, true)
            .expect("encode comp-3");
        let decoded = decode_packed_decimal(&encoded, digits, scale, true)
            .expect("decode comp-3");
        let re_encoded = encode_packed_decimal(&decoded.to_string(), digits, scale, true)
            .expect("re-encode comp-3");

        prop_assert_eq!(&encoded, &re_encoded,
            "COMP-3 encode roundtrip mismatch for {}", value_str);
    }
}

// ============================================================================
// 3. COMP (BINARY): random integer → encode → decode → compare
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Signed 16-bit binary: value → encode → decode → identical.
    #[test]
    fn prop_encode_rt_comp_binary_i16(value in i64::from(i16::MIN)..=i64::from(i16::MAX)) {
        let encoded = encode_binary_int(value, 16, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 16, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Signed 32-bit binary: value → encode → decode → identical.
    #[test]
    fn prop_encode_rt_comp_binary_i32(value in i64::from(i32::MIN)..=i64::from(i32::MAX)) {
        let encoded = encode_binary_int(value, 32, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 32, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Signed 64-bit binary: value → encode → decode → identical.
    #[test]
    fn prop_encode_rt_comp_binary_i64(value in any::<i64>()) {
        let encoded = encode_binary_int(value, 64, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 64, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }
}

// ============================================================================
// 4. COMP-1 (float): random f32 → encode → decode → within epsilon
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Finite f32 → IEEE BE encode → decode → bitwise identical.
    #[test]
    fn prop_encode_rt_comp1_float(raw_bits in any::<u32>()) {
        let value = f32::from_bits(raw_bits);
        if !value.is_finite() {
            return Ok(());
        }
        let mut buffer = [0u8; 4];
        encode_float_single(value, &mut buffer).expect("encode comp-1");
        let decoded = decode_float_single(&buffer).expect("decode comp-1");
        prop_assert_eq!(
            value.to_bits(), decoded.to_bits(),
            "COMP-1 roundtrip: {} != {}", value, decoded
        );
    }
}

// ============================================================================
// 5. COMP-2 (double): random f64 → encode → decode → within epsilon
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Finite f64 → IEEE BE encode → decode → bitwise identical.
    #[test]
    fn prop_encode_rt_comp2_double(raw_bits in any::<u64>()) {
        let value = f64::from_bits(raw_bits);
        if !value.is_finite() {
            return Ok(());
        }
        let mut buffer = [0u8; 8];
        encode_float_double(value, &mut buffer).expect("encode comp-2");
        let decoded = decode_float_double(&buffer).expect("decode comp-2");
        prop_assert_eq!(
            value.to_bits(), decoded.to_bits(),
            "COMP-2 roundtrip: {} != {}", value, decoded
        );
    }
}

// ============================================================================
// 6. SIGN SEPARATE: random signed values → encode → decode → compare
// ============================================================================

fn sign_sep_encode_strategy() -> impl Strategy<Value = (String, u16, i16, bool)> {
    (1u16..=9, 0i16..=3, any::<bool>()).prop_flat_map(|(digits, scale, negative)| {
        #[allow(clippy::cast_possible_wrap)]
        let scale = scale.min((digits as i16) - 1).max(0);
        let max_val = 10i64.saturating_pow(u32::from(digits)) - 1;
        (0..=max_val, Just(digits), Just(scale), Just(negative)).prop_map(
            move |(abs, digits, scale, negative)| {
                let sign = if negative && abs != 0 { "-" } else { "" };
                let value_str = if scale > 0 {
                    let width = usize::from(digits);
                    let s = format!("{abs:0>width$}");
                    let sc = usize::try_from(scale).unwrap_or(0);
                    let split = s.len().saturating_sub(sc);
                    let (int, frac) = s.split_at(split);
                    let int = if int.is_empty() { "0" } else { int };
                    format!("{sign}{int}.{frac}")
                } else {
                    format!("{sign}{abs}")
                };
                (value_str, digits, scale, negative && abs != 0)
            },
        )
    })
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// SIGN SEPARATE LEADING: encode → decode → re-encode → bytes match.
    #[test]
    fn prop_encode_rt_sign_separate_leading(
        (value, digits, scale, _neg) in sign_sep_encode_strategy(),
    ) {
        let info = SignSeparateInfo { placement: SignPlacement::Leading };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, Codepage::ASCII, &mut buffer)
            .expect("encode");
        let decoded = decode_zoned_decimal_sign_separate(&buffer, digits, scale, &info, Codepage::ASCII)
            .expect("decode");

        let mut buffer2 = vec![0u8; buf_len];
        encode_zoned_decimal_sign_separate(&decoded.to_string(), digits, scale, &info, Codepage::ASCII, &mut buffer2)
            .expect("re-encode");

        prop_assert_eq!(&buffer, &buffer2,
            "SIGN SEPARATE LEADING encode roundtrip mismatch for {}", value);
    }

    /// SIGN SEPARATE TRAILING: encode → decode → re-encode → bytes match.
    #[test]
    fn prop_encode_rt_sign_separate_trailing(
        (value, digits, scale, _neg) in sign_sep_encode_strategy(),
    ) {
        let info = SignSeparateInfo { placement: SignPlacement::Trailing };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, Codepage::CP037, &mut buffer)
            .expect("encode");
        let decoded = decode_zoned_decimal_sign_separate(&buffer, digits, scale, &info, Codepage::CP037)
            .expect("decode");

        let mut buffer2 = vec![0u8; buf_len];
        encode_zoned_decimal_sign_separate(&decoded.to_string(), digits, scale, &info, Codepage::CP037, &mut buffer2)
            .expect("re-encode");

        prop_assert_eq!(&buffer, &buffer2,
            "SIGN SEPARATE TRAILING encode roundtrip mismatch for {}", value);
    }
}

// ============================================================================
// 7. Zoned decimal: random values → encode → decode → compare
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Unsigned zoned decimal: encode → decode → value identical.
    #[test]
    fn prop_encode_rt_zoned_unsigned(
        digits in 1u16..=9,
        raw_value in 0u64..=999_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let value = raw_value % max_val;
        let value_str = value.to_string();

        let encoded = encode_zoned_decimal(&value_str, digits, 0, false, Codepage::ASCII)
            .expect("encode zoned");
        let decoded = decode_zoned_decimal(&encoded, digits, 0, false, Codepage::ASCII, false)
            .expect("decode zoned");

        prop_assert_eq!(decoded.value, i64::try_from(value).unwrap(),
            "zoned unsigned roundtrip mismatch for {}", value);
    }

    /// Signed zoned decimal: encode → decode → re-encode → bytes match.
    #[test]
    fn prop_encode_rt_zoned_signed(
        digits in 1u16..=9,
        raw_value in 0u64..=999_999_999u64,
        negative in any::<bool>(),
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;
        let sign = if negative && abs_val != 0 { "-" } else { "" };
        let value_str = format!("{sign}{abs_val}");

        let encoded = encode_zoned_decimal(&value_str, digits, 0, true, Codepage::ASCII)
            .expect("encode zoned signed");
        let decoded = decode_zoned_decimal(&encoded, digits, 0, true, Codepage::ASCII, false)
            .expect("decode zoned signed");

        let re_encoded = encode_zoned_decimal(&decoded.to_string(), digits, 0, true, Codepage::ASCII)
            .expect("re-encode");

        prop_assert_eq!(&encoded, &re_encoded,
            "zoned signed encode roundtrip mismatch for {}", value_str);
    }
}

// ============================================================================
// 8. Multi-field record: JSON → encode → decode → compare
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Mixed alphanumeric + numeric record: JSON → encode → decode → all fields match.
    #[test]
    fn prop_encode_rt_multi_field_record(
        alpha_text in "[A-Z]{1,8}",
        numeric_val in 0u32..=99999,
    ) {
        let copybook = "\
       01 REC.\n\
           05 FLD-A PIC X(8).\n\
           05 FLD-N PIC 9(5).";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({
            "REC": {
                "FLD-A": alpha_text,
                "FLD-N": numeric_val.to_string(),
            }
        });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        prop_assert_eq!(binary.len(), 13, "record length must be 8+5=13");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        // Alpha field: truncated/padded to 8, then trimmed on decode
        let out_alpha = json_out.get("FLD-A").and_then(|v| v.as_str()).expect("FLD-A");
        let expected_alpha = if alpha_text.len() > 8 {
            &alpha_text[..8]
        } else {
            &alpha_text
        };
        prop_assert_eq!(out_alpha.trim_end(), expected_alpha.trim_end());

        // Numeric field
        let out_num = json_out
            .get("FLD-N")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD-N");
        prop_assert_eq!(out_num, i64::from(numeric_val));
    }
}

// ============================================================================
// 9. Encode never panics on valid JSON input
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Encoding arbitrary strings into PIC X fields never panics.
    #[test]
    fn prop_encode_no_panic_pic_x(
        len in 1usize..=50,
        text in "[ -~]{0,60}",
    ) {
        let copybook = format!("01 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");
        let json_in = serde_json::json!({ "FLD": text });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &json_in, &encode_opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked for PIC X({})", len);
    }

    /// Encoding arbitrary numeric strings into PIC 9 fields never panics.
    #[test]
    fn prop_encode_no_panic_pic_9(
        digits in 1usize..=9,
        text in "[0-9\\-\\.]{0,15}",
    ) {
        let copybook = format!("01 FLD PIC 9({digits}).");
        let schema = parse_copybook(&copybook).expect("parse");
        let json_in = serde_json::json!({ "FLD": text });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &json_in, &encode_opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked for PIC 9({})", digits);
    }
}

// ============================================================================
// 10. Encode → decode identity for lossless types
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// COMP-3 full-stack: JSON → encode → decode → value matches.
    #[test]
    fn prop_encode_rt_comp3_fullstack_identity(
        digits in prop::collection::vec(0u8..=9, 1..=7),
    ) {
        let num_digits = digits.len();
        let value: u64 = digits.iter().fold(0u64, |acc, &d| acc * 10 + u64::from(d));
        let value_str = value.to_string();

        let copybook = format!(
            "       01 REC.\n           05 FLD PIC S9({num_digits}) COMP-3."
        );
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": value_str } });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD value");

        // Max representable value for the digit count
        let max_val = 10i64.saturating_pow(num_digits as u32) - 1;
        let expected = i64::try_from(value).unwrap_or(0).min(max_val);
        prop_assert_eq!(out_val, expected,
            "COMP-3 identity: encoded {} but decoded {}", value_str, out_val);
    }

    /// BINARY full-stack: JSON → encode → decode → value matches.
    #[test]
    fn prop_encode_rt_binary_fullstack_identity(value in -32768i64..=32767) {
        let copybook = "       01 REC.\n           05 FLD PIC S9(4) BINARY.";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD numeric");
        prop_assert_eq!(out_val, value);
    }
}

// ============================================================================
// 11. All codepages produce same-length output
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Encoding the same JSON with different codepages yields same byte length.
    #[test]
    fn prop_encode_rt_all_codepages_same_length(
        text in "[A-Z0-9 ]{1,20}",
        num_val in 0u32..=9999,
    ) {
        let copybook = "\
       01 REC.\n\
           05 FLD-A PIC X(20).\n\
           05 FLD-N PIC 9(4).";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({
            "REC": {
                "FLD-A": text,
                "FLD-N": num_val.to_string(),
            }
        });

        let codepages = [
            Codepage::ASCII,
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];

        let mut lengths = Vec::with_capacity(codepages.len());
        for &cp in &codepages {
            let opts = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(cp);
            let binary = encode_record(&schema, &json_in, &opts).expect("encode");
            lengths.push(binary.len());
        }

        let first = lengths[0];
        for (i, &len) in lengths.iter().enumerate() {
            prop_assert_eq!(len, first,
                "codepage {:?} produced {} bytes vs {} from {:?}",
                codepages[i], len, first, codepages[0]);
        }
    }
}

// ============================================================================
// 12. Multi-type record: all field types in one schema → encode → decode
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Record with PIC X, PIC 9, COMP-3, BINARY fields → encode → decode → values match.
    #[test]
    fn prop_encode_rt_mixed_types_record(
        alpha in "[A-Z]{1,10}",
        display_val in 0u32..=9999,
        comp3_val in 0i32..=9999999,
        binary_val in -32768i32..=32767,
    ) {
        let copybook = "\
       01 REC.\n\
           05 FLD-X  PIC X(10).\n\
           05 FLD-D  PIC 9(4).\n\
           05 FLD-P  PIC S9(7) COMP-3.\n\
           05 FLD-B  PIC S9(4) BINARY.";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({
            "REC": {
                "FLD-X": alpha,
                "FLD-D": display_val.to_string(),
                "FLD-P": comp3_val.to_string(),
                "FLD-B": binary_val.to_string(),
            }
        });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        // BINARY field
        let out_bin = json_out
            .get("FLD-B")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD-B");
        prop_assert_eq!(out_bin, i64::from(binary_val));

        // COMP-3 field
        let out_p = json_out
            .get("FLD-P")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD-P");
        prop_assert_eq!(out_p, i64::from(comp3_val));

        // DISPLAY numeric field
        let out_d = json_out
            .get("FLD-D")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD-D");
        prop_assert_eq!(out_d, i64::from(display_val));
    }
}
