use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};

fn main() {
    // Test the failing case: digits = 2, neg = true -> "-11"
    let s = "-11";

    let schema_text = "
       01 REC.
          05 A PIC S9(2) COMP-3.
    ";

    let schema = copybook_core::parse_copybook(schema_text).unwrap();

    let enc = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
    };

    let dec = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number_mode: copybook_codec::JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: copybook_codec::RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    let v = serde_json::json!({"A": s});
    println!("Original JSON: {}", v);

    // Encode to binary
    let encoded = encode_record(&schema, &v, &enc).unwrap();
    println!("Encoded bytes: {:02x?}", encoded);

    // Decode back to JSON
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    println!("Decoded JSON: {}", decoded);

    // Check values
    let decoded_str = decoded["A"].as_str().unwrap();
    let original_val: f64 = s.parse().unwrap();
    let decoded_val: f64 = decoded_str.parse().unwrap();

    println!("Original value: {}", original_val);
    println!("Decoded value: {}", decoded_val);
    println!("Difference: {}", (original_val - decoded_val).abs());
    println!("EPSILON: {}", f64::EPSILON);
    println!("Test passes: {}", (original_val - decoded_val).abs() < f64::EPSILON);
}
