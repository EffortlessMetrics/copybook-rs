use copybook_core::parse_copybook;

fn main() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).
"#;

    println!("Parsing copybook:");
    println!("{}", copybook);
    
    match parse_copybook(copybook) {
        Ok(schema) => {
            println!("Successfully parsed!");
            println!("Fields: {:?}", schema.fields.len());
            for field in &schema.fields {
                println!("Field: {}", field.name);
                for child in &field.children {
                    println!("  Child: {} (occurs: {:?})", child.name, child.occurs);
                }
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}