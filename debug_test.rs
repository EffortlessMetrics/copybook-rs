use copybook_codec::options::{DecodeOptions, Codepage, RecordFormat, JsonNumberMode, RawMode, UnmappablePolicy};
use copybook_core::parse_copybook;
use std::io::Cursor;

fn main() {
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
    let schema = parse_copybook(copybook).unwrap();
    println!("Schema: {:?}", schema);
    
    let mut options = DecodeOptions::default();
    options.codepage = Codepage::ASCII;
    options.format = RecordFormat::Fixed;
    
    let data = b"12}"; // 3 bytes = 123 with positive sign
    let input = Cursor::new(data);
    let mut output = Vec::new();
    
    println!("Input data: {:?}", data);
    println!("Expected record length: {}", schema.resolved_layout().record_size());
    
    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    println!("Decode result: {:?}", result);
    
    let output_str = String::from_utf8(output).unwrap();
    println!("Output: '{}'", output_str);
}