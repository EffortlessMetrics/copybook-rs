use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage, JsonNumberMode, RecordFormat, RawMode, UnmappablePolicy};

fn main() {
    let copybook = "01 VARIABLE-RECORD PIC X(20).";
    let schema = parse_copybook(copybook).unwrap();
    
    println!("Schema: {:?}", schema);
    
    let options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::RecordRDW,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };
    
    // This is the payload from the RDW record (after stripping the 4-byte header)
    let payload = b"ORIGINAL"; // The 8 bytes from the test
    
    println!("Payload: {:?}", payload);
    println!("Payload as string: {:?}", String::from_utf8_lossy(payload));
    
    match decode_record(&schema, payload, &options) {
        Ok(json_value) => {
            println!("SUCCESS: {:?}", json_value);
            println!("JSON: {}", serde_json::to_string_pretty(&json_value).unwrap());
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
            println!("Error code: {:?}", e.code);
            println!("Error message: {}", e.message);
        }
    }
}
