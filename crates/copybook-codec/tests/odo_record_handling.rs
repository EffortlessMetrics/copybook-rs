#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]

use anyhow::{Result, bail};
use copybook_codec::{Codepage, DecodeOptions, decode_record};
use copybook_core::{ErrorCode, parse_copybook};

const ODO_COPYBOOK: &str = r"
       01 RECORD.
          05 ARRAY-COUNT PIC 9(1).
          05 DYNAMIC-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON ARRAY-COUNT.
             10 ELEMENT PIC X(10).
";

fn create_ebcdic_record(count: u8, data: &[u8]) -> Vec<u8> {
    let mut record = Vec::new();

    // Convert decimal digit to EBCDIC zoned numeric (CP037)
    let ebcdic_digit = match count {
        1 => 0xF1,
        2 => 0xF2,
        3 => 0xF3,
        4 => 0xF4,
        5 => 0xF5,
        9 => 0xF9,
        _ => 0xF0, // Default for 0 and invalid values
    };

    record.push(ebcdic_digit);
    record.extend_from_slice(data);
    record
}

#[test]
fn test_odo_array_valid_count() -> Result<()> {
    let schema = parse_copybook(ODO_COPYBOOK)?;
    let valid_record = create_ebcdic_record(
        3,
        &[
            // Element 1: 10 bytes
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
            // Element 2: 10 bytes
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
            // Element 3: 10 bytes
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    let json_value = decode_record(&schema, &valid_record, &options)?;
    match json_value["ARRAY-COUNT"].as_i64() {
        Some(value) => assert_eq!(value, 3),
        None => assert_eq!(json_value["ARRAY-COUNT"].as_str(), Some("3")),
    }
    assert_eq!(
        json_value["DYNAMIC-ARRAY"].as_array().map(Vec::len),
        Some(3)
    );
    Ok(())
}

#[test]
fn test_odo_array_count_clipped() -> Result<()> {
    let schema = parse_copybook(ODO_COPYBOOK)?;
    let clipped_record = create_ebcdic_record(
        5,
        &[
            // Only provide data for 1 element, but count says 5 (insufficient data)
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    let err = match decode_record(&schema, &clipped_record, &options) {
        Ok(value) => bail!("Expected clipped record to fail, got {value:?}"),
        Err(err) => err,
    };

    assert_eq!(
        err.code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Wrong error code for insufficient data"
    );
    Ok(())
}

#[test]
fn test_odo_array_record_too_short() -> Result<()> {
    let schema = parse_copybook(ODO_COPYBOOK)?;
    let too_short_record = vec![0xF3]; // Just count, no data

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    let err = match decode_record(&schema, &too_short_record, &options) {
        Ok(value) => bail!("Expected too-short record to fail, got {value:?}"),
        Err(err) => err,
    };

    assert_eq!(
        err.code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Wrong error code for too short record"
    );
    Ok(())
}

#[test]
fn test_odo_array_count_raised() -> Result<()> {
    let schema = parse_copybook(ODO_COPYBOOK)?;
    let raised_count_record = create_ebcdic_record(
        9,
        &[
            // Provide data for 5 elements (max), but count says 9 (exceeds max 5)
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4,
            0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
            0xF9, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2,
            0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true);

    let err = match decode_record(&schema, &raised_count_record, &options) {
        Ok(value) => bail!("Expected raised ODO count to fail, got {value:?}"),
        Err(err) => err,
    };

    assert_eq!(
        err.code,
        ErrorCode::CBKS301_ODO_CLIPPED,
        "Wrong error code for ODO count exceeding maximum"
    );
    Ok(())
}

#[test]
fn test_odo_array_count_below_min() -> Result<()> {
    let schema = parse_copybook(ODO_COPYBOOK)?;
    let below_min_record = create_ebcdic_record(
        0,
        &[
            // Provide some data (but count 0 is below min 1)
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true);

    let err = match decode_record(&schema, &below_min_record, &options) {
        Ok(value) => bail!("Expected ODO count below minimum to fail, got {value:?}"),
        Err(err) => err,
    };

    assert_eq!(
        err.code,
        ErrorCode::CBKS301_ODO_CLIPPED,
        "Wrong error code for ODO count below minimum"
    );
    Ok(())
}
