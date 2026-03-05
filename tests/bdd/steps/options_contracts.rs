// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::{
    DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
};
use copybook_zoned_format::ZonedEncodingFormat;
use cucumber::{given, then, when};

use crate::world::CopybookWorld;

#[given(expr = "default decode options")]
async fn given_default_decode_options(world: &mut CopybookWorld) {
    world.decode_options = Some(DecodeOptions::new());
}

#[given(expr = "default encode options")]
async fn given_default_encode_options(world: &mut CopybookWorld) {
    world.encode_options = Some(EncodeOptions::new());
}

#[when(expr = "decode record format is set to {string}")]
async fn when_decode_record_format_set(world: &mut CopybookWorld, format: String) {
    let record_format = parse_record_format(&format);
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_format(record_format));
    }
}

#[when(expr = "decode json number mode is set to {string}")]
async fn when_decode_json_mode_set(world: &mut CopybookWorld, mode: String) {
    let json_mode = parse_json_number_mode(&mode);
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_json_number_mode(json_mode));
    }
}

#[when(expr = "decode raw mode is set to {string}")]
async fn when_decode_raw_mode_set(world: &mut CopybookWorld, mode: String) {
    let raw_mode = parse_raw_mode(&mode);
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_emit_raw(raw_mode));
    }
}

#[then(expr = "decode options format should be {string}")]
async fn then_decode_options_format(world: &mut CopybookWorld, expected: String) {
    let expected = parse_record_format(&expected);
    let actual = world
        .decode_options
        .as_ref()
        .expect("decode options should be set")
        .format;
    assert_eq!(actual, expected);
}

#[then(expr = "decode options json number mode should be {string}")]
async fn then_decode_options_json_mode(world: &mut CopybookWorld, expected: String) {
    let expected = parse_json_number_mode(&expected);
    let actual = world
        .decode_options
        .as_ref()
        .expect("decode options should be set")
        .json_number_mode;
    assert_eq!(actual, expected);
}

#[then(expr = "decode options raw mode should be {string}")]
async fn then_decode_options_raw_mode(world: &mut CopybookWorld, expected: String) {
    let expected = parse_raw_mode(&expected);
    let actual = world
        .decode_options
        .as_ref()
        .expect("decode options should be set")
        .emit_raw;
    assert_eq!(actual, expected);
}

#[when(expr = "encode zoned override is set to {string}")]
async fn when_encode_zoned_override_set(world: &mut CopybookWorld, override_value: String) {
    let zoned_override = parse_zoned_override(&override_value);
    world.ensure_encode_options();
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_zoned_encoding_override(zoned_override));
    }
}

#[when(expr = "encode float format is set to {string}")]
async fn when_encode_float_format_set(world: &mut CopybookWorld, float_format: String) {
    let format = parse_float_format(&float_format);
    world.ensure_encode_options();
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_float_format(format));
    }
}

#[then(expr = "encode options zoned override should be {string}")]
async fn then_encode_zoned_override(world: &mut CopybookWorld, expected: String) {
    let expected = parse_zoned_override(&expected);
    let actual = world
        .encode_options
        .as_ref()
        .expect("encode options should be set")
        .zoned_encoding_override;
    assert_eq!(actual, expected);
}

#[then(expr = "encode options float format should be {string}")]
async fn then_encode_float_format(world: &mut CopybookWorld, expected: String) {
    let expected = parse_float_format(&expected);
    let actual = world
        .encode_options
        .as_ref()
        .expect("encode options should be set")
        .float_format;
    assert_eq!(actual, expected);
}

#[when(expr = "zoned encoding is detected from byte {string}")]
async fn when_detect_zoned_encoding(world: &mut CopybookWorld, byte_literal: String) {
    let byte = parse_byte_literal(&byte_literal);
    world.detected_zoned_encoding = ZonedEncodingFormat::detect_from_byte(byte);
}

#[then(expr = "detected zoned encoding should be {string}")]
async fn then_detected_zoned_encoding(world: &mut CopybookWorld, expected: String) {
    let expected = parse_zoned_encoding(&expected);
    assert_eq!(world.detected_zoned_encoding, Some(expected));
}

fn parse_record_format(value: &str) -> RecordFormat {
    match value.to_ascii_lowercase().as_str() {
        "fixed" => RecordFormat::Fixed,
        "rdw" => RecordFormat::RDW,
        _ => panic!("Unknown record format: {value}"),
    }
}

fn parse_json_number_mode(value: &str) -> JsonNumberMode {
    match value.to_ascii_lowercase().as_str() {
        "lossless" => JsonNumberMode::Lossless,
        "native" => JsonNumberMode::Native,
        _ => panic!("Unknown json number mode: {value}"),
    }
}

fn parse_raw_mode(value: &str) -> RawMode {
    match value.to_ascii_lowercase().as_str() {
        "off" => RawMode::Off,
        "record" => RawMode::Record,
        "field" => RawMode::Field,
        "record+rdw" | "record_rdw" | "recordrdw" => RawMode::RecordRDW,
        _ => panic!("Unknown raw mode: {value}"),
    }
}

fn parse_zoned_encoding(value: &str) -> ZonedEncodingFormat {
    match value.to_ascii_lowercase().as_str() {
        "ascii" => ZonedEncodingFormat::Ascii,
        "ebcdic" => ZonedEncodingFormat::Ebcdic,
        "auto" => ZonedEncodingFormat::Auto,
        _ => panic!("Unknown zoned encoding format: {value}"),
    }
}

fn parse_zoned_override(value: &str) -> Option<ZonedEncodingFormat> {
    match value.to_ascii_lowercase().as_str() {
        "none" => None,
        other => Some(parse_zoned_encoding(other)),
    }
}

fn parse_float_format(value: &str) -> FloatFormat {
    match value.to_ascii_lowercase().as_str() {
        "ieee-be" | "ieee" => FloatFormat::IeeeBigEndian,
        "ibm-hex" | "ibm" => FloatFormat::IbmHex,
        _ => panic!("Unknown float format: {value}"),
    }
}

fn parse_byte_literal(value: &str) -> u8 {
    if let Some(hex) = value
        .strip_prefix("0x")
        .or_else(|| value.strip_prefix("0X"))
    {
        u8::from_str_radix(hex, 16).unwrap_or_else(|_| panic!("Invalid hex byte: {value}"))
    } else {
        value
            .parse::<u8>()
            .unwrap_or_else(|_| panic!("Invalid decimal byte: {value}"))
    }
}
