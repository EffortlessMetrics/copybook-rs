use copybook_core::parse_copybook;
use copybook_codec::{DecodeOptions, RawMode, Codepage, JsonNumberMode, RecordFormat, UnmappablePolicy, decode_file_to_jsonl};
use std::io::Cursor;

fn main() {
    let copybook = r#"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
"#;

    let schema = parse_copybook(copybook).unwrap();
    
    let decode_options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Record, // This should add __raw_b64
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };

    let test_data = b"HELLO123";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    decode_file_to_jsonl(&schema, input, &mut output, &decode_options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    println!("Decoded JSON: {}", output_str.trim());

    let json_record: serde_json::Value = serde_json::from_str(output_str.trim()).unwrap();
    println!("Has __raw_b64: {}", json_record.get("__raw_b64").is_some());
    if let Some(raw) = json_record.get("__raw_b64") {
        println!("__raw_b64 value: {}", raw);
    }
}
