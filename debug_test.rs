use copybook_codec::{decode_file_to_jsonl, Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;
use std::io::Cursor;

fn main() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;
    let schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };
    
    // Counter = 99 (exceeds max of 5)
    // RDW header: length=17, reserved=0, followed by payload
    let test_data = b"\x00\x11\x00\x0099ABCDEFGHIJKLMNO"; // Counter + 5 array elements
    let input = Cursor::new(test_data);
    let mut output = Vec::new();
    
    println!("Test data: {:?}", test_data);
    println!("Test data as hex: {}", hex::encode(test_data));
    
    let summary = decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    println!("Summary: {:?}", summary);
    
    let output_str = String::from_utf8(output).unwrap();
    println!("Raw output:\n{}", output_str);
    println!("Raw output repr: {:?}", output_str);
}
