use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;

fn main() {
    let copybook = r"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ORIGINAL-AREA PIC X(20).
   05 REDEFINE-AREA REDEFINES ORIGINAL-AREA.
      10 PART1 PIC X(10).
      10 PART2 PIC X(10).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(4).
";

    let schema = parse_copybook(copybook).expect("Valid copybook should parse");
    println!("Schema LRECL: {:?}", schema.lrecl_fixed);
    println!("Schema tail_odo: {:?}", schema.tail_odo);

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

    // Test data: counter=3, original area, 3 array items
    let test_data = b"003HELLO WORLD      ITEM1ITEM2ITEM3";
    println!("Input data length: {}", test_data.len());

    match copybook_codec::decode_record(&schema, test_data, &options) {
        Ok(json_value) => {
            println!("decode_record SUCCESS!");
            println!(
                "JSON: {}",
                serde_json::to_string_pretty(&json_value).expect("JSON serialization should work")
            );
        }
        Err(e) => {
            println!("decode_record FAILED: {e:?}");
        }
    }
}
