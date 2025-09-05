let copybook = r#"
01 PACKED-RECORD.
   05 PACKED-ODD PIC 9(5) COMP-3.
   05 PACKED-EVEN PIC 9(6) COMP-3.
   05 PACKED-SIGNED PIC S9(3) COMP-3.
"#;
let schema = copybook_core::parse_copybook(copybook).unwrap();
println!("Schema lrecl_fixed: {:?}", schema.lrecl_fixed);
println!("Total fields: {}", schema.fields.len());
for field in &schema.fields {
    println!("Field: {} offset={} len={}", field.name, field.offset, field.len);
}
