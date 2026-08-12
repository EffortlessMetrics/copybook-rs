// SPDX-License-Identifier: AGPL-3.0-or-later
//! Regression coverage for checked zoned-decimal magnitude accumulation.

use anyhow::{Result, bail, ensure};
use copybook_codec::numeric::{
    decode_zoned_decimal, decode_zoned_decimal_to_string_with_scratch,
    decode_zoned_decimal_with_encoding, decode_zoned_decimal_with_scratch,
};
use copybook_codec::runtime::ScratchBuffers;
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record,
    decode_record_with_scratch,
};
use copybook_core::{Error, ErrorCode, parse_copybook};

const I64_MAX_DIGITS: &[u8] = b"9223372036854775807";
const I64_OVERFLOW_DIGITS: &[u8] = b"9223372036854775808";

fn require_error<T>(result: copybook_core::Result<T>) -> Result<Error> {
    match result {
        Ok(_) => bail!("expected zoned-decimal overflow"),
        Err(error) => Ok(error),
    }
}

fn ensure_overflow(error: &Error) -> Result<()> {
    ensure!(
        error.code == ErrorCode::CBKD410_ZONED_OVERFLOW,
        "expected CBKD410_ZONED_OVERFLOW, got {}",
        error.code
    );
    Ok(())
}

fn ebcdic_unsigned(ascii_digits: &[u8]) -> Vec<u8> {
    ascii_digits
        .iter()
        .map(|byte| 0xF0 + (byte - b'0'))
        .collect()
}

#[test]
fn basic_decode_accepts_i64_max_and_rejects_next_magnitude() -> Result<()> {
    let maximum = decode_zoned_decimal(I64_MAX_DIGITS, 19, 0, false, Codepage::ASCII, false)?;
    ensure!(maximum.to_string() == "9223372036854775807");

    let overflow = require_error(decode_zoned_decimal(
        I64_OVERFLOW_DIGITS,
        19,
        0,
        false,
        Codepage::ASCII,
        false,
    ))?;
    ensure_overflow(&overflow)
}

#[test]
fn encoding_preserving_decode_checks_overflow_with_preservation_on_and_off() -> Result<()> {
    for preserve_encoding in [false, true] {
        let (maximum, encoding) = decode_zoned_decimal_with_encoding(
            I64_MAX_DIGITS,
            19,
            0,
            false,
            Codepage::ASCII,
            false,
            preserve_encoding,
        )?;
        ensure!(maximum.to_string() == "9223372036854775807");
        ensure!(encoding.is_some() == preserve_encoding);

        let overflow = require_error(decode_zoned_decimal_with_encoding(
            I64_OVERFLOW_DIGITS,
            19,
            0,
            false,
            Codepage::ASCII,
            false,
            preserve_encoding,
        ))?;
        ensure_overflow(&overflow)?;
    }
    Ok(())
}

#[test]
fn scratch_and_string_paths_propagate_checked_overflow() -> Result<()> {
    let mut scratch = ScratchBuffers::new();
    let maximum = decode_zoned_decimal_with_scratch(
        I64_MAX_DIGITS,
        19,
        2,
        false,
        Codepage::ASCII,
        false,
        &mut scratch,
    )?;
    ensure!(maximum.to_string() == "92233720368547758.07");

    let overflow = require_error(decode_zoned_decimal_with_scratch(
        I64_OVERFLOW_DIGITS,
        19,
        0,
        false,
        Codepage::ASCII,
        false,
        &mut scratch,
    ))?;
    ensure_overflow(&overflow)?;

    let string_overflow = require_error(decode_zoned_decimal_to_string_with_scratch(
        I64_OVERFLOW_DIGITS,
        19,
        0,
        false,
        Codepage::ASCII,
        false,
        &mut scratch,
    ))?;
    ensure_overflow(&string_overflow)
}

#[test]
fn sign_and_codepage_do_not_change_magnitude_capacity() -> Result<()> {
    let mut ascii_positive_max = I64_MAX_DIGITS.to_vec();
    ascii_positive_max[18] = b'G';
    let mut ascii_negative_max = I64_MAX_DIGITS.to_vec();
    ascii_negative_max[18] = b'P';
    let mut ascii_negative_overflow = I64_OVERFLOW_DIGITS.to_vec();
    ascii_negative_overflow[18] = b'Q';

    for (data, expected) in [
        (ascii_positive_max.as_slice(), "9223372036854775807"),
        (ascii_negative_max.as_slice(), "-9223372036854775807"),
    ] {
        let decoded = decode_zoned_decimal(data, 19, 0, true, Codepage::ASCII, false)?;
        ensure!(decoded.to_string() == expected);
    }
    ensure_overflow(&require_error(decode_zoned_decimal(
        &ascii_negative_overflow,
        19,
        0,
        true,
        Codepage::ASCII,
        false,
    ))?)?;

    let ebcdic_max = ebcdic_unsigned(I64_MAX_DIGITS);
    let ebcdic_overflow = ebcdic_unsigned(I64_OVERFLOW_DIGITS);
    let decoded = decode_zoned_decimal(&ebcdic_max, 19, 0, false, Codepage::CP037, false)?;
    ensure!(decoded.to_string() == "9223372036854775807");
    ensure_overflow(&require_error(decode_zoned_decimal(
        &ebcdic_overflow,
        19,
        0,
        false,
        Codepage::CP037,
        false,
    ))?)?;

    let mut ebcdic_negative_max = ebcdic_max;
    ebcdic_negative_max[18] = 0xD7;
    let mut ebcdic_negative_overflow = ebcdic_overflow;
    ebcdic_negative_overflow[18] = 0xD8;
    let decoded = decode_zoned_decimal(&ebcdic_negative_max, 19, 0, true, Codepage::CP037, false)?;
    ensure!(decoded.to_string() == "-9223372036854775807");
    ensure_overflow(&require_error(decode_zoned_decimal(
        &ebcdic_negative_overflow,
        19,
        0,
        true,
        Codepage::CP037,
        false,
    ))?)?;
    Ok(())
}

#[test]
fn leading_zero_width_may_exceed_nineteen_when_magnitude_fits() -> Result<()> {
    let decoded = decode_zoned_decimal(
        b"09223372036854775807",
        20,
        0,
        false,
        Codepage::ASCII,
        false,
    )?;
    ensure!(decoded.to_string() == "9223372036854775807");
    Ok(())
}

#[test]
fn decode_record_pic_9_19_preserves_typed_field_context() -> Result<()> {
    let schema = parse_copybook("01 REC. 05 AMOUNT PIC 9(19).")?;
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    let standard = require_error(decode_record(&schema, I64_OVERFLOW_DIGITS, &options))?;
    ensure_overflow(&standard)?;
    let standard_context = standard
        .context
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("standard decode omitted overflow context"))?;
    ensure!(standard_context.record_index == Some(0));
    ensure!(standard_context.field_path.as_deref() == Some("REC.AMOUNT"));
    ensure!(standard_context.byte_offset == Some(0));

    let mut scratch = ScratchBuffers::new();
    let scratch_error = require_error(decode_record_with_scratch(
        &schema,
        I64_OVERFLOW_DIGITS,
        &options,
        &mut scratch,
    ))?;
    ensure_overflow(&scratch_error)?;
    let scratch_context = scratch_error
        .context
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("scratch decode omitted overflow context"))?;
    ensure!(scratch_context.record_index == Some(0));
    ensure!(scratch_context.field_path.as_deref() == Some("REC.AMOUNT"));
    ensure!(scratch_context.byte_offset == Some(0));
    Ok(())
}
