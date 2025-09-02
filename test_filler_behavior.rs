use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};

fn main() {
    let copybook = r#"
01 RECORD-NAME.
   05 FIELD1 PIC X(5).
   05 FILLER PIC X(3).
   05 FIELD2 PIC X(5).
   05 FILLER PIC X(2).
"#;

    // Test default behavior (FILLER fields not emitted)
    println!("=== Default behavior (emit_filler: false) ===");
    let schema_default = parse_copybook(copybook).unwrap();
    let root = &schema_default.fields[0];
    println!("Number of children: {}", root.children.len());
    for (i, child) in root.children.iter().enumerate() {
        println!("Child {}: name='{}', offset={}, len={}", i, child.name, child.offset, child.len);
    }

    // Test with FILLER fields emitted
    println!("\n=== With FILLER fields emitted (emit_filler: true) ===");
    let schema_with_filler = parse_copybook_with_options(copybook, &ParseOptions { emit_filler: true, ..Default::default() }).unwrap();
    let root = &schema_with_filler.fields[0];
    println!("Number of children: {}", root.children.len());
    for (i, child) in root.children.iter().enumerate() {
        println!("Child {}: name='{}', offset={}, len={}", i, child.name, child.offset, child.len);
    }
}