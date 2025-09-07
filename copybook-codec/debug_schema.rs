use copybook_core::parse_copybook;

fn main() {
    let copybook = "01 VARIABLE-RECORD PIC X(20).";
    match parse_copybook(copybook) {
        Ok(schema) => {
            println!("Schema fields:");
            for field in &schema.fields {
                println!("  Field: name='{}', path='{}', offset={}, len={}, kind={:?}",
                    field.name, field.path, field.offset, field.len, field.kind);
                for child in &field.children {
                    println!("    Child: name='{}', path='{}', offset={}, len={}, kind={:?}",
                        child.name, child.path, child.offset, child.len, child.kind);
                }
            }
        }
        Err(e) => println!("Parse error: {:?}", e)
    }
}
