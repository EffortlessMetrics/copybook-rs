// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_options::{
    CodecOptionKind, FloatFormat, JsonNumberMode, ParseCodecOptionError, RawMode, RecordFormat,
};
use std::{error::Error, io, str::FromStr};

fn requires_typed_error<T>()
where
    T: FromStr<Err = ParseCodecOptionError>,
{
}

#[test]
fn encoding_compatibility_package_forwards_typed_parse_contract() -> Result<(), Box<dyn Error>> {
    requires_typed_error::<FloatFormat>();
    requires_typed_error::<RecordFormat>();
    requires_typed_error::<JsonNumberMode>();
    requires_typed_error::<RawMode>();

    let Err(error) = RecordFormat::from_str("stream") else {
        return Err(io::Error::other("expected unsupported record format").into());
    };

    assert_eq!(error.kind(), CodecOptionKind::RecordFormat);
    assert_eq!(error.input(), "stream");
    assert_eq!(error.accepted_spellings(), &["fixed", "rdw"]);
    assert_eq!(error.to_string(), "unsupported record format `stream`");
    Ok(())
}
