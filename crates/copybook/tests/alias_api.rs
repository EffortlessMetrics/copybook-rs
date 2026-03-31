// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook::{Codepage, DecodeOptions, RecordFormat, decode_record, parse_copybook};

#[test]
fn alias_package_reexports_facade_api() {
    let schema = parse_copybook("01 REC.\n   05 FLAG PIC X(1).\n").expect("schema should parse");
    let options = DecodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed);

    let json = decode_record(&schema, b"Z", &options).expect("decode should succeed");
    assert_eq!(json["FLAG"], "Z");
}
