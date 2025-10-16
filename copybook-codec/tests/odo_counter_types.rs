// ODO counter type tests - currently disabled as ODO support is not implemented
// in the main decode path (json.rs just returns max_count for ODO fields).
#![allow(dead_code, unused_imports)]

use anyhow::{Context, Result};
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use serde_json::Value;

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(true)
        .with_max_errors(None)
        .with_unmappable_policy(UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto)
}

#[test]
fn test_odo_zoned_counter() -> Result<()> {
    let copybook = r"
01 RECORD.
   05 ITEM-COUNT PIC 9(2).
   05 ITEMS PIC X(3) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
";

    let schema = parse_copybook(copybook).context("Failed to parse zoned ODO counter copybook")?;
    let options = decode_opts();

    let data = b"03AAABBBCCC";
    let json = copybook_codec::decode_record(&schema, data, &options)
        .context("Failed to decode zoned ODO record")?;
    let items = json
        .get("ITEMS")
        .and_then(|v| v.as_array())
        .context("ITEMS array missing from zoned decode result")?;
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String("AAA".into()));
    assert_eq!(items[1], Value::String("BBB".into()));
    assert_eq!(items[2], Value::String("CCC".into()));
    Ok(())
}

#[test]
fn test_odo_packed_counter() -> Result<()> {
    let copybook = r"
01 RECORD.
   05 ITEM-COUNT PIC 9(2) COMP-3.
   05 ITEMS PIC X(3) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
";

    let schema = parse_copybook(copybook).context("Failed to parse packed ODO counter copybook")?;
    let options = decode_opts();

    // Packed decimal representation of 03 with PIC 9(2): [padding=0][digit=0][digit=3][sign=F]
    // Bytes: 0x00 (high=0 padding, low=0 first digit), 0x3F (high=3 second digit, low=F sign)
    let mut data = vec![0x00, 0x3F];
    data.extend_from_slice(b"AAABBBCCC");
    let json = copybook_codec::decode_record(&schema, &data, &options)
        .context("Failed to decode packed ODO record")?;
    let items = json
        .get("ITEMS")
        .and_then(|v| v.as_array())
        .context("ITEMS array missing from packed decode result")?;
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String("AAA".into()));
    assert_eq!(items[1], Value::String("BBB".into()));
    assert_eq!(items[2], Value::String("CCC".into()));
    Ok(())
}

#[test]
fn test_odo_binary_counter() -> Result<()> {
    let copybook = r"
01 RECORD.
   05 ITEM-COUNT PIC 9(4) COMP.
   05 ITEMS PIC X(3) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
";

    let schema = parse_copybook(copybook).context("Failed to parse binary ODO counter copybook")?;
    let options = decode_opts();

    let mut data = vec![0x00, 0x03];
    data.extend_from_slice(b"AAABBBCCC");
    let json = copybook_codec::decode_record(&schema, &data, &options)
        .context("Failed to decode binary ODO record")?;
    let items = json
        .get("ITEMS")
        .and_then(|v| v.as_array())
        .context("ITEMS array missing from binary decode result")?;
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String("AAA".into()));
    assert_eq!(items[1], Value::String("BBB".into()));
    assert_eq!(items[2], Value::String("CCC".into()));
    Ok(())
}
