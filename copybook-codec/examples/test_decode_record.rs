use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let copybook = r"
01 RECORD-LAYOUT.
   05 RECORD-ID     PIC 9(3).
   05 CUSTOMER-NAME PIC X(20).
   05 ITEM-COUNT    PIC 9(1).
   05 ITEM-LIST     OCCURS 3 TIMES PIC X(4).
";

    let mut schema = parse_copybook(copybook)?;
    schema.lrecl_fixed = Some(36);

    println!("Schema LRECL: {:?}", schema.lrecl_fixed);

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_unmappable_policy(UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);

    // Test data: record id, customer name, count, and three line items
    let test_data = b"003HELLO WORLD         3ITEMITEMITEM";
    println!("Input data length: {}", test_data.len());

    match copybook_codec::decode_record(&schema, test_data, &options) {
        Ok(json_value) => {
            println!("decode_record SUCCESS!");
            match serde_json::to_string_pretty(&json_value) {
                Ok(pretty) => println!("JSON: {pretty}"),
                Err(format_error) => {
                    eprintln!("Failed to format JSON output: {format_error}");
                }
            }
        }
        Err(e) => {
            eprintln!("decode_record FAILED: {e:?}");
        }
    }

    Ok(())
}
