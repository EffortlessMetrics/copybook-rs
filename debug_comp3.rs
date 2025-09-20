use copybook_codec::encode_packed_decimal;

fn main() {
    let result = encode_packed_decimal("123.4", 3, 2, true);
    match result {
        Ok(_) => println!("Success - unexpected!"),
        Err(e) => println!("Error message: '{}'", e.message),
    }
}