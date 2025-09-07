use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy, decode_record
};
use copybook_core::parse_copybook;
use std::io::Cursor;

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

    let schema = parse_copybook(copybook).unwrap();
    println!("Schema parsed successfully:");
    println!("LRECL: {:?}", schema.lrecl_fixed);
    
    for (i, field) in schema.fields.iter().enumerate() {
        println!("Field {}: {} at offset {} len {} kind {:?}", i, field.name, field.offset, field.len, field.kind);
        if let Some(occurs) = &field.occurs {
            println!("  Has OCCURS: {:?}", occurs);
        }
        for (j, child) in field.children.iter().enumerate() {
            println!("  Child {}: {} at offset {} len {} kind {:?}", j, child.name, child.offset, child.len, child.kind);
        }
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

    // Test data: counter=3, original area, 3 array items
    let test_data = b"003HELLO WORLD      ITEM1ITEM2ITEM3";
    println!("Test data length: {}", test_data.len());
    println!("Test data: {:?}", std::str::from_utf8(test_data).unwrap_or("invalid utf8"));

    // Try direct decode_record first
    println!("\n=== Direct decode_record test ===");
    match decode_record(&schema, test_data, &options) {
        Ok(json_value) => {
            println!("SUCCESS: {}", serde_json::to_string_pretty(&json_value).unwrap());
        }
        Err(e) => {
            println!("FAILED: {}", e);
            if let Some(context) = &e.context {
                println!("Context: {:?}", context);
            }
        }
    }

    // Now try decode_file_to_jsonl
    println!("\n=== decode_file_to_jsonl test ===");
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    match copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options) {
        Ok(summary) => {
            println!("Summary: records_processed={}, errors={}, warnings={}", 
                     summary.records_processed, summary.records_with_errors, summary.warnings);
            let output_str = String::from_utf8(output).unwrap();
            println!("Output: {}", output_str);
        }
        Err(e) => {
            println!("FAILED: {}", e);
            if let Some(context) = &e.context {
                println!("Context: {:?}", context);
            }
        }
    }
}
