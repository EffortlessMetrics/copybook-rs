use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use serde_json::Value;

fn decode_opts() -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    }
}

#[test]
fn test_odo_zoned_counter() {
    let copybook = r#"
01 RECORD.
   05 ITEM-COUNT PIC 9(2).
   05 ITEMS PIC X(3) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = decode_opts();

    let data = b"03AAABBBCCC";
    let json = copybook_codec::decode_record(&schema, data, &options).unwrap();
    let items = json.get("ITEMS").and_then(|v| v.as_array()).unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String("AAA".into()));
    assert_eq!(items[1], Value::String("BBB".into()));
    assert_eq!(items[2], Value::String("CCC".into()));
}

#[test]
fn test_odo_packed_counter() {
    let copybook = r#"
01 RECORD.
   05 ITEM-COUNT PIC 9(2) COMP-3.
   05 ITEMS PIC X(3) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = decode_opts();

    // Packed decimal representation of 3 with two digits: 0x00 0x3C
    let mut data = vec![0x00, 0x3C];
    data.extend_from_slice(b"AAABBBCCC");
    let json = copybook_codec::decode_record(&schema, &data, &options).unwrap();
    let items = json.get("ITEMS").and_then(|v| v.as_array()).unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String("AAA".into()));
    assert_eq!(items[1], Value::String("BBB".into()));
    assert_eq!(items[2], Value::String("CCC".into()));
}

#[test]
fn test_odo_binary_counter() {
    let copybook = r#"
01 RECORD.
   05 ITEM-COUNT PIC 9(4) COMP.
   05 ITEMS PIC X(3) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = decode_opts();

    let mut data = vec![0x00, 0x03];
    data.extend_from_slice(b"AAABBBCCC");
    let json = copybook_codec::decode_record(&schema, &data, &options).unwrap();
    let items = json.get("ITEMS").and_then(|v| v.as_array()).unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String("AAA".into()));
    assert_eq!(items[1], Value::String("BBB".into()));
    assert_eq!(items[2], Value::String("CCC".into()));
}
