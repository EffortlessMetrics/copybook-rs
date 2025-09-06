use copybook_core::parse_copybook;

fn main() {
    let copybook = r#"
01 DECIMAL-FIELDS.
   05 SCALE-0 PIC 9(5) COMP-3.
   05 SCALE-2 PIC 9(5)V99 COMP-3.
   05 SCALE-4 PIC 9(3)V9999 COMP-3.
   05 NEGATIVE-SCALE PIC 9(3)V99 COMP-3.
"#;
    
    let schema = parse_copybook(copybook).unwrap();
    let root = &schema.fields[0];
    
    for (i, field) in root.children.iter().enumerate() {
        println!("{}: name={}, len={}, offset={}, kind={:?}", 
                 i, field.name, field.len, field.offset, field.kind);
    }
}