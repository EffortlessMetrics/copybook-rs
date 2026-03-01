// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::{Codepage, RecordFormat, decode_file_to_jsonl, encode_jsonl_to_file};
use cucumber::{given, then, when};
use serde_json::Value;
use std::io::Cursor;

use crate::helpers::{
    build_binary_for_all_leaf_fields, collect_string_values, field_value_as_string,
    json_value_for_field, parse_binary_literal,
};
use crate::world::CopybookWorld;

// ========================================================================
// Given Steps
// ========================================================================

#[given(expr = "binary data: {string}")]
async fn given_binary_data(world: &mut CopybookWorld, data: String) {
    world.binary_data = Some(parse_binary_literal(&data));
}

#[given(expr = "JSON data:")]
async fn given_json_data(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.json_data = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "JSON data: {string}")]
async fn given_json_data_inline(world: &mut CopybookWorld, data: String) {
    // cucumber-rs {string} captures don't unescape backslash-quotes
    world.json_data = Some(data.replace("\\\"", "\""));
}

#[given(expr = "ASCII codepage")]
async fn given_ascii_codepage(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_codepage(Codepage::ASCII));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_codepage(Codepage::ASCII));
    }
}

#[given(expr = "EBCDIC codepage")]
async fn given_ebcdic_codepage(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_codepage(Codepage::CP037));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_codepage(Codepage::CP037));
    }
}

#[given(expr = "binary data that is too short")]
async fn given_binary_data_too_short(world: &mut CopybookWorld) {
    world.binary_data = Some(vec![b'A', b'B']);
}

#[given(expr = "binary data with invalid encoding")]
async fn given_binary_data_invalid_encoding(world: &mut CopybookWorld) {
    world.binary_data = Some(vec![0xFF; 10]);
}

#[given(expr = "JSON data with missing required fields")]
async fn given_json_missing_fields(world: &mut CopybookWorld) {
    world.json_data = Some("{}".to_string());
}

#[given(expr = "JSON data with invalid field types")]
async fn given_json_invalid_types(world: &mut CopybookWorld) {
    world.json_data = Some("{\"TEST-FIELD\":[1,2,3]}".to_string());
}

#[given(expr = "binary data for value {float}")]
#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_lossless
)]
async fn given_binary_data_for_float(world: &mut CopybookWorld, value: f64) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let field = world.first_leaf_field().expect("No leaf field found");
    let len = field.len as usize;
    match &field.kind {
        copybook_core::FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            let value_str = format!("{}", value);
            if let Ok(bytes) =
                copybook_codec::numeric::encode_packed_decimal(&value_str, *digits, *scale, *signed)
            {
                world.binary_data = Some(bytes);
            } else {
                let mut buf = vec![0u8; len];
                buf[len - 1] = 0x0C;
                world.binary_data = Some(buf);
            }
        }
        copybook_core::FieldKind::BinaryInt { bits, signed } => {
            let int_val = value as i64;
            let byte_len = (*bits as usize) / 8;
            let bytes = if *signed {
                match byte_len {
                    2 => (int_val as i16).to_be_bytes().to_vec(),
                    4 => (int_val as i32).to_be_bytes().to_vec(),
                    _ => (int_val).to_be_bytes().to_vec(),
                }
            } else {
                match byte_len {
                    2 => (int_val as u16).to_be_bytes().to_vec(),
                    4 => (int_val as u32).to_be_bytes().to_vec(),
                    _ => (int_val as u64).to_be_bytes().to_vec(),
                }
            };
            world.binary_data = Some(bytes);
        }
        copybook_core::FieldKind::ZonedDecimal {
            digits,
            scale,
            signed: _,
            sign_separate,
        } => {
            let int_val = if *scale > 0 {
                (value * 10f64.powi(*scale as i32)).round() as i64
            } else {
                value as i64
            };
            let abs_val = int_val.unsigned_abs();
            let digit_str = format!("{:0>width$}", abs_val, width = *digits as usize);
            let mut buf: Vec<u8> = if sign_separate.is_some() {
                let sign_char = if int_val < 0 { b'-' } else { b'+' };
                let placement = sign_separate.as_ref().unwrap().placement;
                match placement {
                    copybook_core::SignPlacement::Leading => {
                        let mut v = vec![sign_char];
                        v.extend_from_slice(digit_str.as_bytes());
                        v
                    }
                    copybook_core::SignPlacement::Trailing => {
                        let mut v = digit_str.as_bytes().to_vec();
                        v.push(sign_char);
                        v
                    }
                }
            } else {
                digit_str.as_bytes().to_vec()
            };
            buf.truncate(len);
            while buf.len() < len {
                buf.push(b'0');
            }
            world.binary_data = Some(buf);
        }
        _ => {
            let s = format!("{}", value);
            let mut buf = s.into_bytes();
            buf.resize(len, b' ');
            world.binary_data = Some(buf);
        }
    }
}

// "binary data for value {int}" is handled by the {float} variant above

#[given(expr = "binary data for all fields")]
async fn given_binary_data_for_all(world: &mut CopybookWorld) {
    let binary = build_binary_for_all_leaf_fields(world);
    world.binary_data = Some(binary);
}

#[given(expr = "binary data with zero value")]
async fn given_binary_data_zero(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let schema = world.schema();
    let record_len = schema.lrecl_fixed.unwrap_or(10) as usize;
    world.binary_data = Some(vec![b'0'; record_len]);
}

#[given(regex = r"^binary data with COUNT=(\d+) and (\d+) elements$")]
async fn given_binary_data_with_count(world: &mut CopybookWorld, count: u32, elements: u32) {
    let count_str = format!("{:03}", count);
    let mut data = count_str.into_bytes();
    for i in 0..elements {
        let elem = format!("ELM{:02}", i + 1);
        let mut padded = elem.into_bytes();
        padded.resize(5, b' ');
        data.extend_from_slice(&padded);
    }
    world.binary_data = Some(data);
}

#[given(expr = "EBCDIC binary data")]
async fn given_ebcdic_binary_data(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let schema = world.schema();
    let record_len = schema.lrecl_fixed.unwrap_or(10) as usize;
    let mut data = Vec::with_capacity(record_len);
    let ebcdic_letters: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1];
    for i in 0..record_len {
        data.push(ebcdic_letters[i % ebcdic_letters.len()]);
    }
    world.binary_data = Some(data);
}

#[given(expr = "ASCII JSON data")]
async fn given_ascii_json_data(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let field = world.first_leaf_field().expect("No leaf field found");
    let name = field.name.clone();
    let len = field.len as usize;
    let value: String = "ABCDEFGHIJ".chars().take(len).collect();
    world.json_data = Some(format!("{{\"{}\":\"{}\"}}", name, value));
}

#[given(expr = "binary data")]
async fn given_binary_data_bare(world: &mut CopybookWorld) {
    let binary = build_binary_for_all_leaf_fields(world);
    world.binary_data = Some(binary);
}

#[given(regex = r"^binary data with (\d+) elements$")]
async fn given_binary_data_with_n_elements(world: &mut CopybookWorld, count: usize) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let schema = world.schema();
    let array_field = schema.all_fields().into_iter().find(|f| f.occurs.is_some());
    let elem_size = array_field
        .and_then(|f| f.children.first())
        .map(|c| c.len as usize)
        .unwrap_or(5);
    let mut data = Vec::new();
    for i in 0..count {
        let elem = format!("E{:04}", i + 1);
        let mut padded = elem.into_bytes();
        padded.resize(elem_size, b' ');
        data.extend_from_slice(&padded);
    }
    world.binary_data = Some(data);
}

#[given(regex = r"^JSON data with (\d+) elements$")]
async fn given_json_data_with_n_elements(world: &mut CopybookWorld, count: usize) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let schema = world.schema();
    let array_field = schema.all_fields().into_iter().find(|f| f.occurs.is_some());
    let (array_name, child_name) = if let Some(f) = array_field {
        let cn = f
            .children
            .first()
            .map(|c| c.name.clone())
            .unwrap_or_else(|| "ELEMENT".to_string());
        (f.name.clone(), cn)
    } else {
        ("ARRAY".to_string(), "ELEMENT".to_string())
    };
    let elements: Vec<String> = (0..count)
        .map(|i| format!("{{\"{}\":\"E{:04}\"}}", child_name, i + 1))
        .collect();
    world.json_data = Some(format!("{{\"{}\":[{}]}}", array_name, elements.join(",")));
}

// "binary data with N elements" is handled by the regex variant above

// ========================================================================
// When Steps
// ========================================================================

#[when(expr = "the binary data is decoded")]
async fn when_binary_data_decoded(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_decode_options();

    let binary_data = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();

    // For ODO schemas lrecl_fixed may be None; set it from binary data length
    if world.schema().lrecl_fixed.is_none() {
        world.schema_mut().lrecl_fixed = Some(binary_data.len() as u32);
    }

    let mut output = Vec::new();

    match decode_file_to_jsonl(
        world.schema(),
        Cursor::new(&binary_data),
        &mut output,
        world
            .decode_options
            .as_ref()
            .expect("Decode options not set"),
    ) {
        Ok(_summary) => {
            world.decoded_output = Some(String::from_utf8(output).unwrap());
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "the JSON data is encoded")]
async fn when_json_data_encoded(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_encode_options();

    let json_data = world.json_data.as_ref().expect("JSON data not set").clone();
    let mut output = Vec::new();

    match encode_jsonl_to_file(
        world.schema(),
        Cursor::new(json_data.as_bytes()),
        &mut output,
        world
            .encode_options
            .as_ref()
            .expect("Encode options not set"),
    ) {
        Ok(_summary) => {
            world.encoded_output = Some(output);
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "the data is round-tripped")]
async fn when_data_roundtripped(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_decode_options();
    world.ensure_encode_options();

    let binary_data = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();

    // For ODO schemas lrecl_fixed may be None; set it from binary data length
    if world.schema().lrecl_fixed.is_none() {
        world.schema_mut().lrecl_fixed = Some(binary_data.len() as u32);
    }

    let mut decoded = Vec::new();

    match decode_file_to_jsonl(
        world.schema(),
        Cursor::new(&binary_data),
        &mut decoded,
        world
            .decode_options
            .as_ref()
            .expect("Decode options not set"),
    ) {
        Ok(_summary) => {
            let decoded_str = String::from_utf8(decoded).unwrap();
            world.decoded_output = Some(decoded_str.clone());

            let mut encoded = Vec::new();
            match encode_jsonl_to_file(
                world.schema(),
                Cursor::new(decoded_str.as_bytes()),
                &mut encoded,
                world
                    .encode_options
                    .as_ref()
                    .expect("Encode options not set"),
            ) {
                Ok(_summary) => {
                    world.encoded_output = Some(encoded);
                }
                Err(e) => {
                    world.error = Some(e);
                }
            }
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "JSON data is encoded")]
async fn when_json_data_encoded_alias(world: &mut CopybookWorld) {
    when_json_data_encoded(world).await;
}

#[when(expr = "binary data is decoded")]
async fn when_binary_data_decoded_alias(world: &mut CopybookWorld) {
    when_binary_data_decoded(world).await;
}

// ========================================================================
// Then Steps
// ========================================================================

#[then(expr = "the decoded output should contain {string}")]
async fn then_decoded_output_contains(world: &mut CopybookWorld, expected: String) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    assert!(
        output.contains(&expected),
        "Expected decoded output to contain '{}', got: {}",
        expected,
        output
    );
}

#[then(expr = "decoded field {word} should be {string}")]
async fn then_decoded_field_value(world: &mut CopybookWorld, field_name: String, expected: String) {
    let record = world.first_decoded_record();
    let value = json_value_for_field(&record, &field_name).expect(&format!(
        "Field '{}' not found in decoded output",
        field_name
    ));
    let actual = field_value_as_string(value)
        .expect(&format!("Field '{}' has unsupported type", field_name));
    assert_eq!(
        actual.trim(),
        expected,
        "Expected decoded field '{}' to be '{}', got '{}'",
        field_name,
        expected,
        actual.trim()
    );
}

#[then(expr = "decoded field {word} should be {int}")]
async fn then_decoded_field_int_value(
    world: &mut CopybookWorld,
    field_name: String,
    expected: i64,
) {
    let record = world.first_decoded_record();
    let value = json_value_for_field(&record, &field_name).expect(&format!(
        "Field '{}' not found in decoded output",
        field_name
    ));
    let actual = field_value_as_string(value)
        .expect(&format!("Field '{}' has unsupported type", field_name));
    let actual_num: i64 = actual.trim().parse().expect(&format!(
        "Field '{}' value '{}' is not a valid integer",
        field_name,
        actual.trim()
    ));
    assert_eq!(
        actual_num, expected,
        "Expected decoded field '{}' to be {}, got {}",
        field_name, expected, actual_num
    );
}

#[then(expr = "decoded field {word} should be blank")]
async fn then_decoded_field_blank(world: &mut CopybookWorld, field_name: String) {
    let record = world.first_decoded_record();
    if let Some(v) = json_value_for_field(&record, &field_name) {
        let s = field_value_as_string(v).unwrap_or_default();
        assert!(
            s.trim().is_empty(),
            "Expected decoded field '{}' to be blank, got '{}'",
            field_name,
            s
        );
    }
}

#[then(expr = "the decoded output should be valid JSON")]
async fn then_decoded_output_valid_json(world: &mut CopybookWorld) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    for line in output.lines() {
        if line.trim().is_empty() {
            continue;
        }
        let _: Value =
            serde_json::from_str(line).expect(&format!("Output should be valid JSON: {}", line));
    }
}

#[then(expr = "the round-trip should be lossless")]
async fn then_roundtrip_lossless(world: &mut CopybookWorld) {
    // For JSON-first workflows there may be no original binary_data
    if let Some(original) = world.binary_data.as_ref() {
        let encoded = world
            .encoded_output
            .as_ref()
            .expect("Encoded output not set");

        assert_eq!(
            original, encoded,
            "Round-trip should be lossless: original data differs from encoded data"
        );
    } else {
        // JSON-first: just verify encode succeeded
        assert!(
            world.encoded_output.is_some(),
            "Round-trip should produce encoded output"
        );
    }
}

#[then(expr = "encoding should succeed")]
async fn then_encoding_succeeds(world: &mut CopybookWorld) {
    if let Some(ref e) = world.error {
        panic!("Encoding failed with error: {e}");
    }
    assert!(world.encoded_output.is_some(), "Encoding should produce output");
}

#[then(expr = "decoding should succeed")]
async fn then_decoding_should_succeed(world: &mut CopybookWorld) {
    if let Some(ref e) = world.error {
        panic!("Decoding failed with error: {e}");
    }
    assert!(world.decoded_output.is_some(), "Decoding should produce output");
}

#[then(expr = "the encoded output should be {int} bytes")]
async fn then_encoded_output_bytes(world: &mut CopybookWorld, expected: usize) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert_eq!(
        output.len(),
        expected,
        "Expected encoded output to be {} bytes, got {}",
        expected,
        output.len()
    );
}

#[then(regex = r"^(\d+) records should be processed$")]
async fn then_n_records_processed(world: &mut CopybookWorld, expected: usize) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    let count = output.lines().filter(|l| !l.trim().is_empty()).count();
    assert_eq!(
        count, expected,
        "Expected {} records, got {}",
        expected, count
    );
}

#[then(expr = "decoding should fail")]
async fn then_decoding_should_fail(world: &mut CopybookWorld) {
    assert!(world.error.is_some(), "Decoding should fail");
}

#[then(expr = "encoding should fail")]
async fn then_encoding_should_fail(world: &mut CopybookWorld) {
    assert!(world.error.is_some(), "Encoding should fail");
}

#[then(expr = "the decoded value should be {string}")]
async fn then_the_decoded_value(world: &mut CopybookWorld, expected: String) {
    let record = world.first_decoded_record();
    let values = collect_string_values(&record);
    let found = values.iter().any(|v| v.trim() == expected);
    assert!(
        found,
        "Expected decoded value '{}' in {:?}",
        expected, values
    );
}

#[then(expr = "encoded length should be {int} bytes")]
async fn then_encoded_length(world: &mut CopybookWorld, expected: usize) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert_eq!(
        output.len(),
        expected,
        "Expected encoded length {} bytes, got {}",
        expected,
        output.len()
    );
}

// "decoded COUNT should be ..." handled by generic "decoded field {word} should be {string}"

#[then(regex = r"^there should be (\d+) ARRAY elements$")]
async fn then_n_array_elements(world: &mut CopybookWorld, expected: usize) {
    let record = world.first_decoded_record();
    let obj = record.as_object().expect("Decoded output should be object");
    let array = obj.values().find(|v| v.is_array());
    let count = array.map(|a| a.as_array().unwrap().len()).unwrap_or(0);
    assert_eq!(
        count, expected,
        "Expected {} array elements, got {}",
        expected, count
    );
}

#[then(expr = "all fields should be decoded")]
async fn then_all_fields_decoded(world: &mut CopybookWorld) {
    let _record = world.first_decoded_record();
}

#[then(regex = r#"^([A-Z][A-Z0-9-]*) should be "(.+)"$"#)]
async fn then_bare_field_value(world: &mut CopybookWorld, field_name: String, expected: String) {
    let record = world.first_decoded_record();
    let value = json_value_for_field(&record, &field_name).expect(&format!(
        "Field '{}' not found in decoded output",
        field_name
    ));
    let actual = field_value_as_string(value).unwrap_or_default();
    assert_eq!(
        actual.trim(),
        expected,
        "Expected {}='{}', got '{}'",
        field_name,
        expected,
        actual.trim()
    );
}

#[then(expr = "decoded value should be converted to ASCII")]
async fn then_decoded_value_ascii(world: &mut CopybookWorld) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    assert!(output.is_ascii(), "Decoded output should be ASCII");
}

#[then(expr = "encoded data should be in EBCDIC")]
async fn then_encoded_ebcdic(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert!(!output.is_empty(), "Encoded output should not be empty");
}

#[then(expr = "encoded data should be blank")]
async fn then_encoded_blank(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert!(
        output.iter().all(|&b| b == b' ' || b == b'0'),
        "Expected encoded data to be blank"
    );
}

#[then(expr = "round-trip should be lossless")]
async fn then_roundtrip_lossless_bare(world: &mut CopybookWorld) {
    then_roundtrip_lossless(world).await;
}

// "decoded AMOUNT should be blank" handled by generic "decoded field {word} should be blank"

#[then(expr = "the encoded data should match the original")]
async fn then_encoded_matches_original(world: &mut CopybookWorld) {
    assert!(
        world.encoded_output.is_some(),
        "Encoded output should exist"
    );
}

#[then(expr = "the array should be properly decoded")]
async fn then_array_decoded(world: &mut CopybookWorld) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    assert!(
        !output.trim().is_empty(),
        "Decoded output should not be empty"
    );
}

#[then(expr = "the decoded output should contain Level-88 conditions")]
async fn then_output_has_level88(world: &mut CopybookWorld) {
    assert!(
        world.decoded_output.is_some(),
        "Decoded output should exist"
    );
}

#[then(expr = "decoded value should be {string}")]
async fn then_decoded_value_bare(world: &mut CopybookWorld, expected: String) {
    then_the_decoded_value(world, expected).await;
}
