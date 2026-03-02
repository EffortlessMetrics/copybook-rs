// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-options.
//!
//! Covers all option types, sensible defaults, builder patterns,
//! serialization/deserialization, and invalid option handling.

#![allow(clippy::unwrap_used)]

use copybook_options::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};

// ====================================================================
// 1. Builder idempotency — setting same value twice yields same result
// ====================================================================

#[test]
fn decode_builder_idempotent_on_repeated_calls() {
    let opts = DecodeOptions::new().with_threads(4).with_threads(8);
    assert_eq!(opts.threads, 8, "last call wins");
}

#[test]
fn encode_builder_idempotent_on_repeated_calls() {
    let opts = EncodeOptions::new()
        .with_max_errors(Some(10))
        .with_max_errors(Some(20));
    assert_eq!(opts.max_errors, Some(20));
}

// ====================================================================
// 2. Builder returns Self (chaining works)
// ====================================================================

#[test]
fn decode_builder_full_chain_compiles_and_produces_correct_values() {
    let opts = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP1047)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_filler(true)
        .with_emit_meta(true)
        .with_emit_raw(RawMode::RecordRDW)
        .with_strict_mode(true)
        .with_max_errors(Some(100))
        .with_unmappable_policy(UnmappablePolicy::Skip)
        .with_threads(16)
        .with_preserve_zoned_encoding(true)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic)
        .with_float_format(FloatFormat::IbmHex);

    assert_eq!(opts.format, RecordFormat::RDW);
    assert_eq!(opts.codepage, Codepage::CP1047);
    assert_eq!(opts.json_number_mode, JsonNumberMode::Native);
    assert!(opts.emit_filler);
    assert!(opts.emit_meta);
    assert_eq!(opts.emit_raw, RawMode::RecordRDW);
    assert!(opts.strict_mode);
    assert_eq!(opts.max_errors, Some(100));
    assert_eq!(opts.on_decode_unmappable, UnmappablePolicy::Skip);
    assert_eq!(opts.threads, 16);
    assert!(opts.preserve_zoned_encoding);
    assert_eq!(opts.preferred_zoned_encoding, ZonedEncodingFormat::Ebcdic);
    assert_eq!(opts.float_format, FloatFormat::IbmHex);
}

#[test]
fn encode_builder_full_chain_compiles_and_produces_correct_values() {
    let opts = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP500)
        .with_use_raw(true)
        .with_bwz_encode(true)
        .with_strict_mode(true)
        .with_max_errors(Some(5))
        .with_threads(2)
        .with_coerce_numbers(true)
        .with_unmappable_policy(UnmappablePolicy::Replace)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ascii)
        .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ebcdic))
        .with_float_format(FloatFormat::IbmHex);

    assert_eq!(opts.format, RecordFormat::RDW);
    assert_eq!(opts.codepage, Codepage::CP500);
    assert!(opts.use_raw);
    assert!(opts.bwz_encode);
    assert!(opts.strict_mode);
    assert_eq!(opts.max_errors, Some(5));
    assert_eq!(opts.threads, 2);
    assert!(opts.coerce_numbers);
    assert_eq!(opts.on_encode_unmappable, UnmappablePolicy::Replace);
    assert_eq!(opts.json_number_mode, JsonNumberMode::Native);
    assert_eq!(opts.preferred_zoned_encoding, ZonedEncodingFormat::Ascii);
    assert_eq!(
        opts.zoned_encoding_override,
        Some(ZonedEncodingFormat::Ebcdic)
    );
    assert_eq!(opts.float_format, FloatFormat::IbmHex);
}

// ====================================================================
// 3. Serde: reject invalid JSON shapes
// ====================================================================

#[test]
fn decode_options_serde_rejects_missing_required_fields() {
    let json = r#"{"format":"Fixed"}"#; // missing many required fields
    let result: Result<DecodeOptions, _> = serde_json::from_str(json);
    assert!(result.is_err());
}

#[test]
fn encode_options_serde_rejects_empty_object() {
    let result: Result<EncodeOptions, _> = serde_json::from_str("{}");
    assert!(result.is_err());
}

#[test]
fn record_format_serde_rejects_unknown_variant() {
    let result: Result<RecordFormat, _> = serde_json::from_str(r#""Chunked""#);
    assert!(result.is_err());
}

#[test]
fn json_number_mode_serde_rejects_unknown_variant() {
    let result: Result<JsonNumberMode, _> = serde_json::from_str(r#""Approx""#);
    assert!(result.is_err());
}

#[test]
fn raw_mode_serde_rejects_unknown_variant() {
    let result: Result<RawMode, _> = serde_json::from_str(r#""Segment""#);
    assert!(result.is_err());
}

#[test]
fn float_format_serde_rejects_unknown_variant() {
    let result: Result<FloatFormat, _> = serde_json::from_str(r#""vax""#);
    assert!(result.is_err());
}

// ====================================================================
// 4. RecordFormat helpers
// ====================================================================

#[test]
fn record_format_is_fixed_and_is_variable_are_mutually_exclusive() {
    for fmt in [RecordFormat::Fixed, RecordFormat::RDW] {
        assert_ne!(fmt.is_fixed(), fmt.is_variable());
    }
}

#[test]
fn record_format_description_is_unique_per_variant() {
    assert_ne!(
        RecordFormat::Fixed.description(),
        RecordFormat::RDW.description()
    );
}

// ====================================================================
// 5. JsonNumberMode helpers
// ====================================================================

#[test]
fn json_number_mode_lossless_and_native_are_mutually_exclusive() {
    for mode in [JsonNumberMode::Lossless, JsonNumberMode::Native] {
        assert_ne!(mode.is_lossless(), mode.is_native());
    }
}

#[test]
fn json_number_mode_description_is_unique_per_variant() {
    assert_ne!(
        JsonNumberMode::Lossless.description(),
        JsonNumberMode::Native.description()
    );
}

// ====================================================================
// 6. Display implementations produce distinct strings for all variants
// ====================================================================

#[test]
fn record_format_display_distinct() {
    let a = RecordFormat::Fixed.to_string();
    let b = RecordFormat::RDW.to_string();
    assert_ne!(a, b);
}

#[test]
fn json_number_mode_display_distinct() {
    let a = JsonNumberMode::Lossless.to_string();
    let b = JsonNumberMode::Native.to_string();
    assert_ne!(a, b);
}

#[test]
fn raw_mode_display_all_distinct() {
    let variants = [
        RawMode::Off,
        RawMode::Record,
        RawMode::Field,
        RawMode::RecordRDW,
    ];
    let displays: Vec<String> = variants.iter().map(ToString::to_string).collect();
    let unique: std::collections::HashSet<&String> = displays.iter().collect();
    assert_eq!(unique.len(), variants.len());
}

#[test]
fn float_format_display_distinct() {
    let a = FloatFormat::IeeeBigEndian.to_string();
    let b = FloatFormat::IbmHex.to_string();
    assert_ne!(a, b);
}

// ====================================================================
// 7. Codepage serde round-trip for all variants
// ====================================================================

#[test]
fn codepage_serde_roundtrip_all_variants() {
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let json = serde_json::to_string(&cp).unwrap();
        let back: Codepage = serde_json::from_str(&json).unwrap();
        assert_eq!(back, cp);
    }
}

// ====================================================================
// 8. Encode convenience method equivalence
// ====================================================================

#[test]
fn encode_with_zoned_encoding_format_equals_override_some() {
    let a = EncodeOptions::new().with_zoned_encoding_format(ZonedEncodingFormat::Ascii);
    let b = EncodeOptions::new().with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii));
    assert_eq!(a.zoned_encoding_override, b.zoned_encoding_override);
}

// ====================================================================
// 9. Serde round-trip preserves None for optional fields
// ====================================================================

#[test]
fn decode_options_max_errors_none_roundtrips() {
    let opts = DecodeOptions::new().with_max_errors(None);
    let json = serde_json::to_string(&opts).unwrap();
    let back: DecodeOptions = serde_json::from_str(&json).unwrap();
    assert!(back.max_errors.is_none());
}

#[test]
fn encode_options_zoned_override_none_roundtrips() {
    let opts = EncodeOptions::new().with_zoned_encoding_override(None);
    let json = serde_json::to_string(&opts).unwrap();
    let back: EncodeOptions = serde_json::from_str(&json).unwrap();
    assert!(back.zoned_encoding_override.is_none());
}

// ====================================================================
// 10. Serde round-trip preserves extreme values
// ====================================================================

#[test]
fn decode_options_max_errors_max_u64_roundtrips() {
    let opts = DecodeOptions::new().with_max_errors(Some(u64::MAX));
    let json = serde_json::to_string(&opts).unwrap();
    let back: DecodeOptions = serde_json::from_str(&json).unwrap();
    assert_eq!(back.max_errors, Some(u64::MAX));
}

#[test]
fn encode_options_threads_zero_roundtrips() {
    let opts = EncodeOptions::new().with_threads(0);
    let json = serde_json::to_string(&opts).unwrap();
    let back: EncodeOptions = serde_json::from_str(&json).unwrap();
    assert_eq!(back.threads, 0);
}

// ====================================================================
// 11. Clone independence
// ====================================================================

#[test]
fn decode_options_clone_independence() {
    let opts = DecodeOptions::new().with_threads(4).with_emit_meta(true);
    let mut cloned = opts.clone();
    cloned.threads = 99;
    cloned.emit_meta = false;
    // Original unchanged
    assert_eq!(opts.threads, 4);
    assert!(opts.emit_meta);
}

#[test]
fn encode_options_clone_independence() {
    let opts = EncodeOptions::new().with_threads(8).with_use_raw(true);
    let mut cloned = opts.clone();
    cloned.threads = 1;
    cloned.use_raw = false;
    assert_eq!(opts.threads, 8);
    assert!(opts.use_raw);
}

// ====================================================================
// 12. Send + Sync
// ====================================================================

#[test]
fn decode_options_is_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<DecodeOptions>();
}

#[test]
fn encode_options_is_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<EncodeOptions>();
}

// ====================================================================
// 13. ZonedEncodingFormat detection edge cases
// ====================================================================

#[test]
fn zoned_encoding_detect_boundary_0x2f_returns_none() {
    // 0x2F is just below the ASCII digit range (0x30-0x39)
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x2F), None);
}

#[test]
fn zoned_encoding_detect_boundary_0x3a_returns_ascii() {
    // 0x3A has high nibble 0x3 → detected as ASCII zone (matches zone nibble)
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0x3A),
        Some(ZonedEncodingFormat::Ascii)
    );
}

#[test]
fn zoned_encoding_detect_boundary_0xef_returns_none() {
    // 0xEF is just below the EBCDIC digit range (0xF0-0xF9)
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0xEF), None);
}
