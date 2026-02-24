// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration-level checks for zoned-format extraction and re-export compatibility.

use copybook_options::ZonedEncodingFormat;
use copybook_zoned_format::ZonedEncodingFormat as Zf;
use serde_json;

#[test]
fn options_reexports_zoned_encoding_format_without_behavior_drift() {
    let codec_level: ZonedEncodingFormat = Zf::Auto;
    let detected = ZonedEncodingFormat::detect_from_byte(0x35);
    let reference = Zf::detect_from_byte(0x35);

    assert_eq!(codec_level, Zf::Auto);
    assert_eq!(detected, Some(ZonedEncodingFormat::Ascii));
    assert_eq!(reference, Some(Zf::Ascii));
    assert_eq!(detected, reference);
}

#[test]
fn options_ser_deser_preserves_zoned_encoding_variants() {
    let options = copybook_options::EncodeOptions::new()
        .with_preferred_zoned_encoding(Zf::Ebcdic)
        .with_zoned_encoding_override(Some(Zf::Ascii));

    let payload = serde_json::to_vec(&options).expect("encode options to json");
    let decoded: copybook_options::EncodeOptions =
        serde_json::from_slice(&payload).expect("decode options from json");

    assert_eq!(decoded.preferred_zoned_encoding, Zf::Ebcdic);
    assert_eq!(decoded.zoned_encoding_override, Some(Zf::Ascii));
}
