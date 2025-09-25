#!/usr/bin/env cargo +stable script

//! Debug test to understand failing comprehensive numeric tests

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;
use serde_json::Value;
use std::io::Cursor;

fn main() {
    // Replicate the exact test conditions from test_packed_decimal_comprehensive
    let copybook = r"
01 PACKED-RECORD.
   05 PACKED-ODD PIC 9(5) COMP-3.
   05 PACKED-EVEN PIC 9(6) COMP-3.
   05 PACKED-SIGNED PIC S9(3) COMP-3.
";

    let schema = match parse_copybook(copybook) {
        Ok(schema) => schema,
        Err(e) => {
            println!("Parse error: {:?}", e);
            return;
        }
    };

    println!("Schema parsed successfully");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_unmappable_policy(UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Ascii);

    // Test data: 12345 (odd), 123456 (even), -123 (signed)
    let test_data = b"\x12\x34\x5F\x12\x34\x56\x0F\x12\x3D";

    println!("Test data: {:02x?}", test_data);

    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    match copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options) {
        Ok(summary) => {
            println!("Decode succeeded. Summary: {:?}", summary);
            let output_str = String::from_utf8(output).unwrap();
            println!("Output string: {:?}", output_str);

            if output_str.trim().is_empty() {
                println!("OUTPUT IS EMPTY!");
                return;
            }

            match serde_json::from_str::<Value>(output_str.trim()) {
                Ok(json_record) => {
                    println!("JSON Record: {:#?}", json_record);
                    println!("PACKED-ODD: {:?}", json_record.get("PACKED-ODD"));
                    println!("PACKED-EVEN: {:?}", json_record.get("PACKED-EVEN"));
                    println!("PACKED-SIGNED: {:?}", json_record.get("PACKED-SIGNED"));
                },
                Err(e) => println!("JSON parse error: {:?}", e)
            }
        },
        Err(e) => {
            println!("Decode error: {:?}", e);
        }
    }
}
