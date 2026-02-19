use base64::Engine;
use copybook_codec::RawMode;
use cucumber::{given, then};

use crate::helpers::parse_binary_literal;
use crate::world::CopybookWorld;

#[given(expr = "raw mode {string}")]
async fn given_raw_mode(world: &mut CopybookWorld, mode_str: String) {
    let mode = match mode_str.to_lowercase().as_str() {
        "off" => RawMode::Off,
        "record" => RawMode::Record,
        "record_rdw" | "recordrdw" => RawMode::RecordRDW,
        "field" => RawMode::Field,
        _ => panic!("Unknown raw mode: {}", mode_str),
    };

    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_emit_raw(mode));
    }
}

#[given(expr = "use_raw enabled for encode")]
async fn given_use_raw_encode(world: &mut CopybookWorld) {
    world.ensure_encode_options();
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_use_raw(true));
    }
}

#[then(expr = "the decoded output should not contain {string}")]
async fn then_decoded_output_not_contain(world: &mut CopybookWorld, expected: String) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    assert!(
        !output.contains(&expected),
        "Expected decoded output to NOT contain '{}', but it did",
        expected
    );
}

#[then(expr = "the raw_b64 field should decode to the original binary data")]
async fn then_raw_b64_matches_original(world: &mut CopybookWorld) {
    let record = world.first_decoded_record();
    let raw_b64 = record
        .get("raw_b64")
        .and_then(|v| v.as_str())
        .expect("raw_b64 field not found in decoded output");

    let decoded_bytes = base64::engine::general_purpose::STANDARD
        .decode(raw_b64)
        .expect("raw_b64 should be valid base64");

    let original = world.binary_data.as_ref().expect("Binary data not set");
    assert_eq!(
        &decoded_bytes, original,
        "raw_b64 decoded bytes should match original binary data"
    );
}

#[then(expr = "the {word} raw bytes should be base64 of {string}")]
async fn then_field_raw_is_base64_of(
    world: &mut CopybookWorld,
    field_name: String,
    expected_hex: String,
) {
    let record = world.first_decoded_record();
    let raw_key = format!("{}__raw_b64", field_name);
    let raw_b64 = record
        .get(&raw_key)
        .and_then(|v| v.as_str())
        .expect(&format!("{} not found in decoded output", raw_key));

    let decoded_bytes = base64::engine::general_purpose::STANDARD
        .decode(raw_b64)
        .expect(&format!("{} should be valid base64", raw_key));

    let expected_bytes = parse_binary_literal(&expected_hex);
    assert_eq!(
        decoded_bytes, expected_bytes,
        "{} decoded bytes should match expected",
        raw_key
    );
}
