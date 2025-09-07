use copybook_core::parse_copybook;
use copybook_codec::{DecodeOptions, RecordFormat, Codepage, JsonNumberMode, RawMode, UnmappablePolicy, decode_file_to_jsonl};
use std::io::Cursor;

fn main() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    println!("Schema fields: {}", schema.fields.len());
    let root = &schema.fields[0];
    println!("Root children: {}", root.children.len());
    
    for (i, child) in root.children.iter().enumerate() {
        println!("Child {}: name={}, occurs={:?}, offset={}, len={}", 
                 i, child.name, child.occurs, child.offset, child.len);
    }

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
    };

    // Counter = 99 (exceeds max of 5)
    let test_data = b"99ABCDEFGHIJKLMNO"; // Counter + 5 array elements
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    println!("Test data: {:?}", test_data);
    println!("Schema LRECL fixed: {:?}", schema.lrecl_fixed);

    let summary = decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    println!("Summary: warnings={}, records_processed={}", summary.warnings, summary.records_processed);
    
    let output_str = String::from_utf8(output).unwrap();
    println!("Output: {}", output_str);
}