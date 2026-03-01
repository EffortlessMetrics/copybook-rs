// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-options configuration types.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_options::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};

// ── RecordFormat ────────────────────────────────────────────────────

#[test]
fn record_format_is_fixed() {
    assert!(RecordFormat::Fixed.is_fixed());
    assert!(!RecordFormat::Fixed.is_variable());
}

#[test]
fn record_format_is_variable() {
    assert!(RecordFormat::RDW.is_variable());
    assert!(!RecordFormat::RDW.is_fixed());
}

#[test]
fn record_format_display() {
    assert_eq!(RecordFormat::Fixed.to_string(), "fixed");
    assert_eq!(RecordFormat::RDW.to_string(), "rdw");
}

#[test]
fn record_format_description_non_empty() {
    assert!(!RecordFormat::Fixed.description().is_empty());
    assert!(!RecordFormat::RDW.description().is_empty());
    assert!(RecordFormat::Fixed.description().contains("Fixed"));
    assert!(RecordFormat::RDW.description().contains("Variable"));
}

#[test]
fn record_format_debug_output() {
    let dbg = format!("{:?}", RecordFormat::Fixed);
    assert!(dbg.contains("Fixed"));
}

#[test]
fn record_format_serde_roundtrip() {
    for fmt in [RecordFormat::Fixed, RecordFormat::RDW] {
        let json = serde_json::to_string(&fmt).unwrap();
        let back: RecordFormat = serde_json::from_str(&json).unwrap();
        assert_eq!(back, fmt);
    }
}

#[test]
fn record_format_clone_and_copy() {
    let a = RecordFormat::Fixed;
    let b = a;
    let c = a.clone();
    assert_eq!(a, b);
    assert_eq!(a, c);
}

// ── JsonNumberMode ──────────────────────────────────────────────────

#[test]
fn json_number_mode_is_lossless() {
    assert!(JsonNumberMode::Lossless.is_lossless());
    assert!(!JsonNumberMode::Lossless.is_native());
}

#[test]
fn json_number_mode_is_native() {
    assert!(JsonNumberMode::Native.is_native());
    assert!(!JsonNumberMode::Native.is_lossless());
}

#[test]
fn json_number_mode_display() {
    assert_eq!(JsonNumberMode::Lossless.to_string(), "lossless");
    assert_eq!(JsonNumberMode::Native.to_string(), "native");
}

#[test]
fn json_number_mode_description_non_empty() {
    assert!(!JsonNumberMode::Lossless.description().is_empty());
    assert!(!JsonNumberMode::Native.description().is_empty());
}

#[test]
fn json_number_mode_serde_roundtrip() {
    for mode in [JsonNumberMode::Lossless, JsonNumberMode::Native] {
        let json = serde_json::to_string(&mode).unwrap();
        let back: JsonNumberMode = serde_json::from_str(&json).unwrap();
        assert_eq!(back, mode);
    }
}

// ── RawMode ─────────────────────────────────────────────────────────

#[test]
fn raw_mode_display() {
    assert_eq!(RawMode::Off.to_string(), "off");
    assert_eq!(RawMode::Record.to_string(), "record");
    assert_eq!(RawMode::Field.to_string(), "field");
    assert_eq!(RawMode::RecordRDW.to_string(), "record+rdw");
}

#[test]
fn raw_mode_serde_roundtrip() {
    for mode in [
        RawMode::Off,
        RawMode::Record,
        RawMode::Field,
        RawMode::RecordRDW,
    ] {
        let json = serde_json::to_string(&mode).unwrap();
        let back: RawMode = serde_json::from_str(&json).unwrap();
        assert_eq!(back, mode);
    }
}

#[test]
fn raw_mode_equality() {
    assert_eq!(RawMode::Off, RawMode::Off);
    assert_ne!(RawMode::Off, RawMode::Record);
    assert_ne!(RawMode::Record, RawMode::RecordRDW);
}

// ── FloatFormat ─────────────────────────────────────────────────────

#[test]
fn float_format_display() {
    assert_eq!(FloatFormat::IeeeBigEndian.to_string(), "ieee-be");
    assert_eq!(FloatFormat::IbmHex.to_string(), "ibm-hex");
}

#[test]
fn float_format_default_is_ieee_be() {
    assert_eq!(FloatFormat::default(), FloatFormat::IeeeBigEndian);
}

#[test]
fn float_format_serde_roundtrip() {
    for ff in [FloatFormat::IeeeBigEndian, FloatFormat::IbmHex] {
        let json = serde_json::to_string(&ff).unwrap();
        let back: FloatFormat = serde_json::from_str(&json).unwrap();
        assert_eq!(back, ff);
    }
}

#[test]
fn float_format_debug_output() {
    let dbg = format!("{:?}", FloatFormat::IbmHex);
    assert!(dbg.contains("IbmHex"));
}

// ── DecodeOptions: defaults ─────────────────────────────────────────

#[test]
fn decode_options_default_values() {
    let opts = DecodeOptions::default();
    assert_eq!(opts.format, RecordFormat::Fixed);
    assert_eq!(opts.codepage, Codepage::CP037);
    assert_eq!(opts.json_number_mode, JsonNumberMode::Lossless);
    assert!(!opts.emit_filler);
    assert!(!opts.emit_meta);
    assert_eq!(opts.emit_raw, RawMode::Off);
    assert!(!opts.strict_mode);
    assert!(opts.max_errors.is_none());
    assert_eq!(opts.on_decode_unmappable, UnmappablePolicy::Error);
    assert_eq!(opts.threads, 1);
    assert!(!opts.preserve_zoned_encoding);
    assert_eq!(opts.preferred_zoned_encoding, ZonedEncodingFormat::Auto);
    assert_eq!(opts.float_format, FloatFormat::IeeeBigEndian);
}

#[test]
fn decode_options_new_equals_default() {
    let new = DecodeOptions::new();
    let def = DecodeOptions::default();
    // Compare key fields
    assert_eq!(new.format, def.format);
    assert_eq!(new.codepage, def.codepage);
    assert_eq!(new.threads, def.threads);
    assert_eq!(new.float_format, def.float_format);
}

// ── DecodeOptions: builder chain ────────────────────────────────────

#[test]
fn decode_options_builder_chain_all_fields() {
    let opts = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP500)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_filler(true)
        .with_emit_meta(true)
        .with_emit_raw(RawMode::Field)
        .with_strict_mode(true)
        .with_max_errors(Some(50))
        .with_unmappable_policy(UnmappablePolicy::Replace)
        .with_threads(8)
        .with_preserve_zoned_encoding(true)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic)
        .with_float_format(FloatFormat::IbmHex);

    assert_eq!(opts.format, RecordFormat::RDW);
    assert_eq!(opts.codepage, Codepage::CP500);
    assert_eq!(opts.json_number_mode, JsonNumberMode::Native);
    assert!(opts.emit_filler);
    assert!(opts.emit_meta);
    assert_eq!(opts.emit_raw, RawMode::Field);
    assert!(opts.strict_mode);
    assert_eq!(opts.max_errors, Some(50));
    assert_eq!(opts.on_decode_unmappable, UnmappablePolicy::Replace);
    assert_eq!(opts.threads, 8);
    assert!(opts.preserve_zoned_encoding);
    assert_eq!(opts.preferred_zoned_encoding, ZonedEncodingFormat::Ebcdic);
    assert_eq!(opts.float_format, FloatFormat::IbmHex);
}

#[test]
fn decode_options_with_max_errors_none() {
    let opts = DecodeOptions::new().with_max_errors(None);
    assert!(opts.max_errors.is_none());
}

#[test]
fn decode_options_with_max_errors_zero() {
    let opts = DecodeOptions::new().with_max_errors(Some(0));
    assert_eq!(opts.max_errors, Some(0));
}

// ── DecodeOptions: Serde roundtrip ──────────────────────────────────

#[test]
fn decode_options_serde_roundtrip_defaults() {
    let opts = DecodeOptions::default();
    let json = serde_json::to_string(&opts).unwrap();
    let back: DecodeOptions = serde_json::from_str(&json).unwrap();
    assert_eq!(back.format, opts.format);
    assert_eq!(back.codepage, opts.codepage);
    assert_eq!(back.threads, opts.threads);
    assert_eq!(back.float_format, opts.float_format);
}

#[test]
fn decode_options_serde_roundtrip_all_fields() {
    let opts = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP1140)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_filler(true)
        .with_emit_meta(true)
        .with_emit_raw(RawMode::RecordRDW)
        .with_strict_mode(true)
        .with_max_errors(Some(999))
        .with_unmappable_policy(UnmappablePolicy::Skip)
        .with_threads(16)
        .with_preserve_zoned_encoding(true)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ascii)
        .with_float_format(FloatFormat::IbmHex);

    let json = serde_json::to_string(&opts).unwrap();
    let back: DecodeOptions = serde_json::from_str(&json).unwrap();

    assert_eq!(back.format, RecordFormat::RDW);
    assert_eq!(back.codepage, Codepage::CP1140);
    assert_eq!(back.json_number_mode, JsonNumberMode::Native);
    assert!(back.emit_filler);
    assert!(back.emit_meta);
    assert_eq!(back.emit_raw, RawMode::RecordRDW);
    assert!(back.strict_mode);
    assert_eq!(back.max_errors, Some(999));
    assert_eq!(back.on_decode_unmappable, UnmappablePolicy::Skip);
    assert_eq!(back.threads, 16);
    assert!(back.preserve_zoned_encoding);
    assert_eq!(back.preferred_zoned_encoding, ZonedEncodingFormat::Ascii);
    assert_eq!(back.float_format, FloatFormat::IbmHex);
}

#[test]
fn decode_options_missing_float_format_defaults_to_ieee() {
    let opts = DecodeOptions::default();
    let mut value = serde_json::to_value(&opts).unwrap();
    value.as_object_mut().unwrap().remove("float_format");
    let back: DecodeOptions = serde_json::from_value(value).unwrap();
    assert_eq!(back.float_format, FloatFormat::IeeeBigEndian);
}

// ── EncodeOptions: defaults ─────────────────────────────────────────

#[test]
fn encode_options_default_values() {
    let opts = EncodeOptions::default();
    assert_eq!(opts.format, RecordFormat::Fixed);
    assert_eq!(opts.codepage, Codepage::CP037);
    assert_eq!(opts.preferred_zoned_encoding, ZonedEncodingFormat::Auto);
    assert!(!opts.use_raw);
    assert!(!opts.bwz_encode);
    assert!(!opts.strict_mode);
    assert!(opts.max_errors.is_none());
    assert_eq!(opts.threads, 1);
    assert!(!opts.coerce_numbers);
    assert_eq!(opts.on_encode_unmappable, UnmappablePolicy::Error);
    assert_eq!(opts.json_number_mode, JsonNumberMode::Lossless);
    assert!(opts.zoned_encoding_override.is_none());
    assert_eq!(opts.float_format, FloatFormat::IeeeBigEndian);
}

#[test]
fn encode_options_new_equals_default() {
    let new = EncodeOptions::new();
    let def = EncodeOptions::default();
    assert_eq!(new.format, def.format);
    assert_eq!(new.codepage, def.codepage);
    assert_eq!(new.threads, def.threads);
}

// ── EncodeOptions: builder chain ────────────────────────────────────

#[test]
fn encode_options_builder_chain_all_fields() {
    let opts = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP273)
        .with_use_raw(true)
        .with_bwz_encode(true)
        .with_strict_mode(true)
        .with_max_errors(Some(10))
        .with_threads(4)
        .with_coerce_numbers(true)
        .with_unmappable_policy(UnmappablePolicy::Replace)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic)
        .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii))
        .with_float_format(FloatFormat::IbmHex);

    assert_eq!(opts.format, RecordFormat::RDW);
    assert_eq!(opts.codepage, Codepage::CP273);
    assert!(opts.use_raw);
    assert!(opts.bwz_encode);
    assert!(opts.strict_mode);
    assert_eq!(opts.max_errors, Some(10));
    assert_eq!(opts.threads, 4);
    assert!(opts.coerce_numbers);
    assert_eq!(opts.on_encode_unmappable, UnmappablePolicy::Replace);
    assert_eq!(opts.json_number_mode, JsonNumberMode::Native);
    assert_eq!(opts.preferred_zoned_encoding, ZonedEncodingFormat::Ebcdic);
    assert_eq!(
        opts.zoned_encoding_override,
        Some(ZonedEncodingFormat::Ascii)
    );
    assert_eq!(opts.float_format, FloatFormat::IbmHex);
}

#[test]
fn encode_options_with_zoned_encoding_format_convenience() {
    let opts = EncodeOptions::new().with_zoned_encoding_format(ZonedEncodingFormat::Ebcdic);
    assert_eq!(
        opts.zoned_encoding_override,
        Some(ZonedEncodingFormat::Ebcdic)
    );
}

#[test]
fn encode_options_zoned_override_none_clears() {
    let opts = EncodeOptions::new()
        .with_zoned_encoding_format(ZonedEncodingFormat::Ascii)
        .with_zoned_encoding_override(None);
    assert!(opts.zoned_encoding_override.is_none());
}

// ── EncodeOptions: Serde roundtrip ──────────────────────────────────

#[test]
fn encode_options_serde_roundtrip_defaults() {
    let opts = EncodeOptions::default();
    let json = serde_json::to_string(&opts).unwrap();
    let back: EncodeOptions = serde_json::from_str(&json).unwrap();
    assert_eq!(back.format, opts.format);
    assert_eq!(back.codepage, opts.codepage);
    assert_eq!(back.threads, opts.threads);
}

#[test]
fn encode_options_serde_roundtrip_all_fields() {
    let opts = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP1047)
        .with_use_raw(true)
        .with_bwz_encode(true)
        .with_strict_mode(true)
        .with_max_errors(Some(42))
        .with_threads(12)
        .with_coerce_numbers(true)
        .with_unmappable_policy(UnmappablePolicy::Skip)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ascii)
        .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ebcdic))
        .with_float_format(FloatFormat::IbmHex);

    let json = serde_json::to_string(&opts).unwrap();
    let back: EncodeOptions = serde_json::from_str(&json).unwrap();

    assert_eq!(back.format, RecordFormat::RDW);
    assert_eq!(back.codepage, Codepage::CP1047);
    assert!(back.use_raw);
    assert!(back.bwz_encode);
    assert!(back.strict_mode);
    assert_eq!(back.max_errors, Some(42));
    assert_eq!(back.threads, 12);
    assert!(back.coerce_numbers);
    assert_eq!(back.on_encode_unmappable, UnmappablePolicy::Skip);
    assert_eq!(back.json_number_mode, JsonNumberMode::Native);
    assert_eq!(back.preferred_zoned_encoding, ZonedEncodingFormat::Ascii);
    assert_eq!(
        back.zoned_encoding_override,
        Some(ZonedEncodingFormat::Ebcdic)
    );
    assert_eq!(back.float_format, FloatFormat::IbmHex);
}

#[test]
fn encode_options_missing_float_format_defaults_to_ieee() {
    let opts = EncodeOptions::default();
    let mut value = serde_json::to_value(&opts).unwrap();
    value.as_object_mut().unwrap().remove("float_format");
    let back: EncodeOptions = serde_json::from_value(value).unwrap();
    assert_eq!(back.float_format, FloatFormat::IeeeBigEndian);
}

// ── Codepage: Display ───────────────────────────────────────────────

#[test]
fn codepage_display_all() {
    assert_eq!(Codepage::CP037.to_string(), "cp037");
    assert_eq!(Codepage::CP273.to_string(), "cp273");
    assert_eq!(Codepage::CP500.to_string(), "cp500");
    assert_eq!(Codepage::CP1047.to_string(), "cp1047");
    assert_eq!(Codepage::CP1140.to_string(), "cp1140");
}

// ── UnmappablePolicy: Display ───────────────────────────────────────

#[test]
fn unmappable_policy_display() {
    assert_eq!(UnmappablePolicy::Error.to_string(), "error");
    assert_eq!(UnmappablePolicy::Replace.to_string(), "replace");
    assert_eq!(UnmappablePolicy::Skip.to_string(), "skip");
}

#[test]
fn unmappable_policy_serde_roundtrip() {
    for policy in [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ] {
        let json = serde_json::to_string(&policy).unwrap();
        let back: UnmappablePolicy = serde_json::from_str(&json).unwrap();
        assert_eq!(back, policy);
    }
}

// ── ZonedEncodingFormat ─────────────────────────────────────────────

#[test]
fn zoned_encoding_format_predicates() {
    assert!(ZonedEncodingFormat::Ascii.is_ascii());
    assert!(!ZonedEncodingFormat::Ascii.is_ebcdic());
    assert!(!ZonedEncodingFormat::Ascii.is_auto());

    assert!(!ZonedEncodingFormat::Ebcdic.is_ascii());
    assert!(ZonedEncodingFormat::Ebcdic.is_ebcdic());
    assert!(!ZonedEncodingFormat::Ebcdic.is_auto());

    assert!(!ZonedEncodingFormat::Auto.is_ascii());
    assert!(!ZonedEncodingFormat::Auto.is_ebcdic());
    assert!(ZonedEncodingFormat::Auto.is_auto());
}

#[test]
fn zoned_encoding_format_display() {
    assert_eq!(ZonedEncodingFormat::Ascii.to_string(), "ascii");
    assert_eq!(ZonedEncodingFormat::Ebcdic.to_string(), "ebcdic");
    assert_eq!(ZonedEncodingFormat::Auto.to_string(), "auto");
}

#[test]
fn zoned_encoding_format_detect_ascii_range() {
    for byte in 0x30..=0x39 {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ascii),
            "byte {byte:#04X}"
        );
    }
}

#[test]
fn zoned_encoding_format_detect_ebcdic_range() {
    for byte in 0xF0..=0xF9 {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ebcdic),
            "byte {byte:#04X}"
        );
    }
}

#[test]
fn zoned_encoding_format_detect_invalid_zones() {
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x00), None);
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x50), None);
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x20), None);
}

// ── DecodeOptions: Debug ────────────────────────────────────────────

#[test]
fn decode_options_debug_output() {
    let opts = DecodeOptions::default();
    let dbg = format!("{:?}", opts);
    assert!(dbg.contains("DecodeOptions"));
    assert!(dbg.contains("format"));
}

// ── EncodeOptions: Debug ────────────────────────────────────────────

#[test]
fn encode_options_debug_output() {
    let opts = EncodeOptions::default();
    let dbg = format!("{:?}", opts);
    assert!(dbg.contains("EncodeOptions"));
    assert!(dbg.contains("format"));
}

// ── Clone behavior ──────────────────────────────────────────────────

#[test]
fn decode_options_clone_is_independent() {
    let opts = DecodeOptions::new().with_threads(4).with_emit_meta(true);
    let cloned = opts.clone();
    assert_eq!(cloned.threads, 4);
    assert!(cloned.emit_meta);
}

#[test]
fn encode_options_clone_is_independent() {
    let opts = EncodeOptions::new().with_threads(8).with_use_raw(true);
    let cloned = opts.clone();
    assert_eq!(cloned.threads, 8);
    assert!(cloned.use_raw);
}
