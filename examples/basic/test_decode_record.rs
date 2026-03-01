// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::{
    Codepage, DecodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
    ZonedEncodingFormat,
};
use copybook_core::parse_copybook;

fn main() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ORIGINAL-AREA PIC X(20).
   05 REDEFINE-AREA REDEFINES ORIGINAL-AREA.
      10 PART1 PIC X(10).
      10 PART2 PIC X(10).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(4).
"#;

    let schema = match parse_copybook(copybook) {
        Ok(schema) => schema,
        Err(error) => {
            eprintln!("Failed to parse copybook schema: {error}");
            return;
        }
    };
    println!("Schema LRECL: {:?}", schema.lrecl_fixed);
    println!("Schema tail_odo: {:?}", schema.tail_odo);

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: FloatFormat::IeeeBigEndian,
    };

    // Test data: counter=3, original area, 3 array items
    let test_data = b"003HELLO WORLD      ITEM1ITEM2ITEM3";
    println!("Input data length: {}", test_data.len());

    match copybook_codec::decode_record(&schema, test_data, &options) {
        Ok(json_value) => {
            println!("decode_record SUCCESS!");
            match serde_json::to_string_pretty(&json_value) {
                Ok(pretty) => println!("JSON: {pretty}"),
                Err(error) => eprintln!("Failed to format JSON output: {error}"),
            }
        }
        Err(error) => {
            eprintln!("decode_record FAILED: {error}");
        }
    }
}
