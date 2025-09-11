use copybook_codec::{decode_file_to_jsonl, Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;
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
        emit_raw: RawMode::Record,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };
    
    let test_data = b"HELLO123";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();
    
    println!("Test data: {:?}", test_data);
    
    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &decode_options).unwrap();
    
    let output_str = String::from_utf8(output).unwrap();
    println!("Raw output:\n{}", output_str);
    println!("Raw output repr: {:?}", output_str);
    
    // Parse and check what we have
    if let Ok(json_record) = serde_json::from_str::<serde_json::Value>(output_str.trim()) {
        println!("Parsed JSON: {}", serde_json::to_string_pretty(&json_record).unwrap());
        println!("Has __raw_b64: {}", json_record.get("__raw_b64").is_some());
    } else {
        println!("Failed to parse as JSON");
    }
}
