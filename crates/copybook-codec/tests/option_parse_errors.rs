// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::{
    CodecOptionKind, FloatFormat, JsonNumberMode, ParseCodecOptionError, RawMode, RecordFormat,
};
use std::{error::Error, io, str::FromStr};

fn parse_error<T>(input: &str) -> Result<ParseCodecOptionError, Box<dyn Error>>
where
    T: FromStr<Err = ParseCodecOptionError>,
{
    match T::from_str(input) {
        Err(error) => Ok(error),
        Ok(_) => Err(io::Error::other(format!("expected `{input}` to be rejected")).into()),
    }
}

fn assert_spellings<T>(
    kind: CodecOptionKind,
    expected: &'static [&'static str],
) -> Result<(), Box<dyn Error>>
where
    T: FromStr<Err = ParseCodecOptionError>,
{
    assert_eq!(kind.accepted_spellings(), expected);
    for spelling in expected {
        if T::from_str(spelling).is_err() {
            return Err(
                io::Error::other(format!("documented spelling `{spelling}` was rejected")).into(),
            );
        }
        let uppercase = spelling.to_ascii_uppercase();
        if T::from_str(&uppercase).is_err() {
            return Err(io::Error::other(format!(
                "case-insensitive spelling `{uppercase}` was rejected"
            ))
            .into());
        }
    }
    Ok(())
}

#[test]
fn encoding_invalid_values_return_inspectable_typed_error() -> Result<(), Box<dyn Error>> {
    let cases = [
        (
            parse_error::<FloatFormat>("FLOAT-WAT")?,
            CodecOptionKind::FloatFormat,
            "FLOAT-WAT",
            "unsupported float format `FLOAT-WAT`",
            &["ieee-be", "ieee", "ieee-big-endian", "ibm-hex", "ibm"][..],
        ),
        (
            parse_error::<RecordFormat>("blocked")?,
            CodecOptionKind::RecordFormat,
            "blocked",
            "unsupported record format `blocked`",
            &["fixed", "rdw"][..],
        ),
        (
            parse_error::<JsonNumberMode>("decimal")?,
            CodecOptionKind::JsonNumberMode,
            "decimal",
            "unsupported JSON number mode `decimal`",
            &["lossless", "native"][..],
        ),
        (
            parse_error::<RawMode>("header")?,
            CodecOptionKind::RawMode,
            "header",
            "unsupported raw mode `header`",
            &["off", "record", "field", "record+rdw"][..],
        ),
    ];

    for (error, kind, input, display, accepted) in cases {
        assert_eq!(error.kind(), kind);
        assert_eq!(error.input(), input);
        assert_eq!(error.to_string(), display);
        assert_eq!(error.accepted_spellings(), accepted);
    }

    Ok(())
}

#[test]
fn encoding_accepted_spellings_remain_complete_and_case_insensitive() -> Result<(), Box<dyn Error>>
{
    assert_spellings::<FloatFormat>(
        CodecOptionKind::FloatFormat,
        &["ieee-be", "ieee", "ieee-big-endian", "ibm-hex", "ibm"],
    )?;
    assert_spellings::<RecordFormat>(CodecOptionKind::RecordFormat, &["fixed", "rdw"])?;
    assert_spellings::<JsonNumberMode>(CodecOptionKind::JsonNumberMode, &["lossless", "native"])?;
    assert_spellings::<RawMode>(
        CodecOptionKind::RawMode,
        &["off", "record", "field", "record+rdw"],
    )?;
    Ok(())
}
