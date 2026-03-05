// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_codec::{
    DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    ZonedEncodingFormat,
};
use copybook_options as options_micro;

fn accept_decode_options(options: options_micro::DecodeOptions) -> options_micro::DecodeOptions {
    options
}

fn accept_encode_options(options: options_micro::EncodeOptions) -> options_micro::EncodeOptions {
    options
}

#[test]
fn codec_decode_options_are_microcrate_types() {
    let codec_options: DecodeOptions = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_raw(RawMode::RecordRDW)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Ascii);

    let micro_options = accept_decode_options(codec_options);
    assert_eq!(micro_options.format, options_micro::RecordFormat::RDW);
    assert_eq!(
        micro_options.json_number_mode,
        options_micro::JsonNumberMode::Native
    );
    assert_eq!(micro_options.emit_raw, options_micro::RawMode::RecordRDW);
}

#[test]
fn codec_encode_options_are_microcrate_types() {
    let codec_options: EncodeOptions = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ebcdic))
        .with_float_format(FloatFormat::IbmHex);

    let micro_options = accept_encode_options(codec_options);
    assert_eq!(micro_options.format, options_micro::RecordFormat::RDW);
    assert_eq!(
        micro_options.zoned_encoding_override,
        Some(options_micro::ZonedEncodingFormat::Ebcdic)
    );
    assert_eq!(
        micro_options.float_format,
        options_micro::FloatFormat::IbmHex
    );
}

#[test]
fn codec_module_path_still_exposes_options_surface() {
    let options = copybook_codec::options::DecodeOptions::new().with_format(RecordFormat::Fixed);
    assert!(options.format.is_fixed());

    let encode = copybook_codec::options::EncodeOptions::new()
        .with_zoned_encoding_format(ZonedEncodingFormat::Ascii);
    assert_eq!(
        encode.zoned_encoding_override,
        Some(copybook_codec::options::ZonedEncodingFormat::Ascii)
    );
}

#[test]
fn serde_roundtrip_is_compatible_through_codec_reexports() {
    let options = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_raw(RawMode::Record);

    let json = serde_json::to_string(&options).expect("serialize decode options");
    let decoded: options_micro::DecodeOptions =
        serde_json::from_str(&json).expect("deserialize as microcrate type");

    assert_eq!(decoded.format, options_micro::RecordFormat::RDW);
    assert_eq!(
        decoded.json_number_mode,
        options_micro::JsonNumberMode::Native
    );
    assert_eq!(decoded.emit_raw, options_micro::RawMode::Record);
}
