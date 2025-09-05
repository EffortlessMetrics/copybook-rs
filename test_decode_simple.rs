use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage};

#[test]
fn test_simple_decode() {
    let copybook = r#"
01 PACKED-RECORD.
   05 PACKED-ODD PIC 9(5) COMP-3.
   05 PACKED-EVEN PIC 9(6) COMP-3.  
   05 PACKED-SIGNED PIC S9(3) COMP-3.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions::default();
    
    println!("Schema lrecl_fixed: {:?}", schema.lrecl_fixed);
    println!("Total fields: {}", schema.fields.len());
    for field in &schema.fields {
        println!("Field: {} offset={} len={} kind={:?}", field.name, field.offset, field.len, field.kind);
        for child in &field.children {
            println!("  Child: {} offset={} len={} kind={:?}", child.name, child.offset, child.len, child.kind);
        }
    }
    
    // Test data: 12345 (odd), 123456 (even), -123 (signed)
    // Expected: 0x12345C, 0x123456C, 0x123D = 9 bytes total
    let test_data = b"\x12\x34\x5C\x12\x34\x56\x0C\x12\x3D";
    
    let json_value = decode_record(&schema, test_data, &options).unwrap();
    println!("Decoded JSON: {}", serde_json::to_string_pretty(&json_value).unwrap());
}