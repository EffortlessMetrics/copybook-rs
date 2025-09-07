use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
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
    println!("Schema parsed successfully");

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
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    println!("Input data length: {}", test_data.len());
    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    
    match result {
        Ok(summary) => {
            println!("Decode successful! Records processed: {}", summary.records_processed);
            println!("Output length: {}", output.len());
            if !output.is_empty() {
                let output_str = String::from_utf8_lossy(&output);
                println!("Output: '{}'", output_str);
            } else {
                println!("Output is empty!");
            }
        }
        Err(e) => {
            println!("Decode failed: {:?}", e);
        }
    }
}
