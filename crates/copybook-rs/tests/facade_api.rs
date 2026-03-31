// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_rs::{
    Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record, parse_copybook,
};

#[test]
fn facade_reexports_core_and_codec_at_crate_root() {
    let schema = parse_copybook("01 REC.\n   05 FLAG PIC X(1).\n").expect("schema should parse");
    let options = DecodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_json_number_mode(JsonNumberMode::Lossless);

    let json = decode_record(&schema, b"A", &options).expect("decode should succeed");
    assert_eq!(json["FLAG"], "A");
}

#[test]
fn facade_keeps_namespaced_modules_available() {
    let schema = copybook_rs::core::parse_copybook("01 REC.\n   05 FLAG PIC X(1).\n")
        .expect("schema should parse");
    let options = copybook_rs::codec::DecodeOptions::new()
        .with_codepage(copybook_rs::codec::Codepage::ASCII)
        .with_format(copybook_rs::codec::RecordFormat::Fixed);

    let json =
        copybook_rs::codec::decode_record(&schema, b"B", &options).expect("decode should succeed");
    assert_eq!(json["FLAG"], "B");
}
