use copybook_core::parse_copybook;

fn main() {
    let copybook = r#"
01 BINARY-RECORD.
   05 UNSIGNED-16 PIC 9(4) COMP.
   05 SIGNED-16 PIC S9(4) COMP.
   05 UNSIGNED-32 PIC 9(9) COMP.
   05 SIGNED-32 PIC S9(9) COMP.
"#;
    let schema = parse_copybook(copybook).unwrap();
    
    println!("Schema fields:");
    for field in &schema.fields {
        println!("  {} - offset: {}, len: {}, kind: {:?}", 
                 field.name, field.offset, field.len, field.kind);
    }
    
    println!("Record length calculation:");
    let calculated_length = schema.fields.iter().map(|f| f.offset + f.len).max().unwrap_or(0);
    println!("  Max field end: {}", calculated_length);
    println!("  LRECL fixed: {:?}", schema.lrecl_fixed);
}