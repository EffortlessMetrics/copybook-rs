// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, DeterminismMode, decode_record, determinism::check_decode_determinism,
};
use copybook_core::parse_copybook;
use copybook_determinism::{DeterminismResult, compare_outputs};

fn assert_reexport_result_type(_: DeterminismResult) {}

#[test]
fn codec_determinism_result_matches_microcrate_engine() {
    let schema = parse_copybook(
        r"
        01 TEST-RECORD.
           05 FIELD-A PIC X(5).
    ",
    )
    .expect("schema should parse");

    let opts = DecodeOptions::new().with_codepage(Codepage::CP037);
    let data = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6]; // HELLO in CP037

    let codec_result =
        check_decode_determinism(&schema, &data, &opts).expect("codec determinism should work");

    let decoded_a = decode_record(&schema, &data, &opts).expect("decode run a");
    let decoded_b = decode_record(&schema, &data, &opts).expect("decode run b");
    let bytes_a = serde_json::to_vec(&decoded_a).expect("serialize run a");
    let bytes_b = serde_json::to_vec(&decoded_b).expect("serialize run b");
    let micro_result = compare_outputs(DeterminismMode::DecodeOnly, &bytes_a, &bytes_b);

    assert_eq!(codec_result, micro_result);
    assert_reexport_result_type(codec_result.clone());
}

#[test]
fn codec_reexports_microcrate_mode_variants() {
    let mode = DeterminismMode::RoundTrip;
    let external_mode: copybook_determinism::DeterminismMode = mode;
    assert_eq!(
        external_mode,
        copybook_determinism::DeterminismMode::RoundTrip
    );
}
