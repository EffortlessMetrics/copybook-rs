// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::decode_zoned_decimal_with_encoding;
use copybook_codec::{Codepage, ZonedEncodingFormat};
use copybook_core::ErrorCode;

#[test]
fn cbkd415_zoned_encoding_ambiguous_is_emitted_for_undetectable_data() {
    let error = decode_zoned_decimal_with_encoding(
        &[0x00, 0x01, 0x02],
        3,
        0,
        false,
        Codepage::CP037,
        false,
        true,
    )
    .expect_err("undetectable zoned bytes should be rejected");

    assert_eq!(error.code, ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS);

    let info = copybook_codec::ZonedEncodingInfo::detect_from_data(&[0x00, 0x01, 0x02])
        .expect("encoding analysis should complete");
    assert_eq!(info.detected_format, ZonedEncodingFormat::Auto);
    assert!(!info.has_mixed_encoding);
}
