use copybook_core::parse_copybook;

fn main() {
    let copybook = r#"
01 PACKED-RECORD.
   05 PACKED-ODD PIC 9(5) COMP-3.
   05 PACKED-EVEN PIC 9(6) COMP-3.
   05 PACKED-SIGNED PIC S9(3) COMP-3.
"#;
    let schema = parse_copybook(copybook).unwrap();
    
    println!("Schema lrecl_fixed: {:?}", schema.lrecl_fixed);
    println!("Total fields: {}", schema.fields.len());
    for field in &schema.fields {
        println!("Field: {} offset={} len={} kind={:?}", field.name, field.offset, field.len, field.kind);
        for child in &field.children {
            println!("  Child: {} offset={} len={} kind={:?}", child.name, child.offset, child.len, child.kind);
        }
    }
}