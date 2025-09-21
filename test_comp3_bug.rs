use copybook_codec::numeric::decode_packed_decimal;

fn main() {
    // Test data from our failing case: -11 should be encoded as 11DD
    let data = vec![0x11, 0xDD];

    println!("Testing decode of bytes: {:02X?}", data);

    // Test with 2 digits, scale 0, signed
    let result = decode_packed_decimal(&data, 2, 0, true).unwrap();

    println!("Decoded value: {}", result.value);
    println!("Scale: {}", result.scale);
    println!("Negative: {}", result.negative);
    println!("to_string(): '{}'", result.to_string());

    // Expected: value=11, scale=0, negative=true, to_string()="-11"

    // Test the actual failing case from property test
    let test_result = result.to_string();
    let expected = "-11";

    println!("Expected: '{}'", expected);
    println!("Actual: '{}'", test_result);
    println!("Match: {}", test_result == expected);
}