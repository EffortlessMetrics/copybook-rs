use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy, decode_record};
use copybook_core::parse_copybook;

fn main() {
    // Test copybook with FILLER fields at various offsets
    let copybook = r#"
01 RECORD-LAYOUT.
   05 FIELD-A PIC X(5).
   05 FILLER PIC X(3).
   05 FIELD-B PIC 9(4).
   05 FILLER PIC X(2).
   05 FIELD-C PIC X(6).
"#;
    
    println!("Testing FILLER byte-offset naming...");
    
    let schema = parse_copybook(copybook).unwrap();
    
    // Print schema field information
    println!("\nSchema fields:");
    for field in &schema.fields[0].children {
        println!("  - Name: '{}', Offset: {}, Length: {}", field.name, field.offset, field.len);
    }
    
    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: true, // Enable FILLER emission
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };
    
    // Test data matching the layout: 5+3+4+2+6 = 20 bytes
    let test_data = b"HELLO   1234  WORLD!";
    
    match decode_record(&schema, test_data, &options) {
        Ok(json_value) => {
            println!("\nDecoded JSON with FILLER fields:");
            println!("{}", serde_json::to_string_pretty(&json_value).unwrap());
            
            // Verify FILLER field naming by byte offset
            if let Some(obj) = json_value.as_object() {
                let has_filler_at_5 = obj.contains_key("_filler_00000005");
                let has_filler_at_12 = obj.contains_key("_filler_00000012");
                
                println!("\nFILLER field naming validation:");
                println!("  - FILLER at offset 5: {} (expected: _filler_00000005)", has_filler_at_5);
                println!("  - FILLER at offset 12: {} (expected: _filler_00000012)", has_filler_at_12);
                
                if has_filler_at_5 && has_filler_at_12 {
                    println!("\n✅ FILLER byte-offset naming feature is working correctly!");
                } else {
                    println!("\n❌ FILLER byte-offset naming feature is not working as expected!");
                }
            }
        }
        Err(e) => {
            println!("❌ Decode failed: {:?}", e);
        }
    }
}