use copybook_core::parse_copybook;

fn main() {
    let simple_copybook = "01 RECORD.\n    05 FIELD-A PIC X(10).";
    let result = parse_copybook(simple_copybook);
    println!("Parsing result: {:?}", result);

    match result {
        Ok(schema) => {
            println!("Successfully parsed. Fields count: {}", schema.fields.len());
            for field in &schema.fields {
                println!("Field: {} at level {}", field.name, field.level);
            }
        }
        Err(e) => {
            println!("Error: {}", e.message);
            println!("Error code: {:?}", e.code);
        }
    }
}