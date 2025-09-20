use copybook_core::{parse_copybook_with_options, ParseOptions};

fn main() {
    let input = r#"01 CUSTOMER-ID PIC X(10). *> This should be rejected"#;
    let options = ParseOptions {
        allow_inline_comments: false,
        ..ParseOptions::default()
    };
    
    println!("Testing inline comment rejection...");
    println!("Input: {}", input);
    println!("Options: {:?}", options);
    
    match parse_copybook_with_options(input, &options) {
        Ok(schema) => {
            println!("ERROR: Parsing succeeded when it should have failed!");
            println!("Field name: {}", schema.fields[0].name);
        }
        Err(e) => {
            println!("SUCCESS: Parsing failed as expected");
            println!("Error: {:?}", e);
        }
    }
}
