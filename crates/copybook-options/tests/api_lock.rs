// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_options::{
    DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    ZonedEncodingFormat,
};

#[test]
fn facade_exposes_decode_options_surface() {
    let options = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_raw(RawMode::RecordRDW)
        .with_preserve_zoned_encoding(true)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ascii)
        .with_float_format(FloatFormat::IbmHex);

    assert_eq!(options.format, RecordFormat::RDW);
    assert_eq!(options.json_number_mode, JsonNumberMode::Native);
    assert_eq!(options.emit_raw, RawMode::RecordRDW);
    assert!(options.preserve_zoned_encoding);
    assert_eq!(options.preferred_zoned_encoding, ZonedEncodingFormat::Ascii);
    assert_eq!(options.float_format, FloatFormat::IbmHex);
}

#[test]
fn facade_exposes_encode_options_surface() {
    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_zoned_encoding_format(ZonedEncodingFormat::Ebcdic)
        .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii))
        .with_float_format(FloatFormat::IbmHex);

    assert_eq!(options.format, RecordFormat::RDW);
    assert_eq!(options.json_number_mode, JsonNumberMode::Native);
    assert_eq!(
        options.zoned_encoding_override,
        Some(ZonedEncodingFormat::Ascii)
    );
    assert_eq!(options.float_format, FloatFormat::IbmHex);
}

#[test]
fn facade_exposes_zoned_detection_surface() {
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0x35),
        Some(ZonedEncodingFormat::Ascii)
    );
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0xF4),
        Some(ZonedEncodingFormat::Ebcdic)
    );
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x20), None);
}
