#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Finish documentation and policy-compliant proof for typed option errors."""

from pathlib import Path


def replace_once(path: Path, old: str, new: str) -> None:
    text = path.read_text(encoding="utf-8")
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{path}: expected one marker, found {count}: {old[:80]!r}")
    path.write_text(text.replace(old, new, 1), encoding="utf-8")


Path("crates/copybook-codec/tests/option_parse_errors.rs").write_text(
    r'''// SPDX-License-Identifier: AGPL-3.0-or-later

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
        Ok(_) => Err(io::Error::other(format!(
            "expected `{input}` to be rejected"
        ))
        .into()),
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
            return Err(io::Error::other(format!(
                "documented spelling `{spelling}` was rejected"
            ))
            .into());
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
fn encoding_accepted_spellings_remain_complete_and_case_insensitive(
) -> Result<(), Box<dyn Error>> {
    assert_spellings::<FloatFormat>(
        CodecOptionKind::FloatFormat,
        &["ieee-be", "ieee", "ieee-big-endian", "ibm-hex", "ibm"],
    )?;
    assert_spellings::<RecordFormat>(CodecOptionKind::RecordFormat, &["fixed", "rdw"])?;
    assert_spellings::<JsonNumberMode>(
        CodecOptionKind::JsonNumberMode,
        &["lossless", "native"],
    )?;
    assert_spellings::<RawMode>(
        CodecOptionKind::RawMode,
        &["off", "record", "field", "record+rdw"],
    )?;
    Ok(())
}
''',
    encoding="utf-8",
)

Path("crates/copybook-options/tests/parse_error_compat.rs").write_text(
    r'''// SPDX-License-Identifier: AGPL-3.0-or-later

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
fn encoding_compatibility_package_forwards_typed_parse_contract(
) -> Result<(), Box<dyn Error>> {
    requires_typed_error::<FloatFormat>();
    requires_typed_error::<RecordFormat>();
    requires_typed_error::<JsonNumberMode>();
    requires_typed_error::<RawMode>();

    let error = match RecordFormat::from_str("stream") {
        Err(error) => error,
        Ok(_) => {
            return Err(io::Error::other("expected unsupported record format").into());
        }
    };

    assert_eq!(error.kind(), CodecOptionKind::RecordFormat);
    assert_eq!(error.input(), "stream");
    assert_eq!(error.accepted_spellings(), &["fixed", "rdw"]);
    assert_eq!(error.to_string(), "unsupported record format `stream`");
    Ok(())
}
''',
    encoding="utf-8",
)

library_section = r'''
### Parsing codec option values

`FloatFormat`, `RecordFormat`, `JsonNumberMode`, and `RawMode` share
`ParseCodecOptionError` as their public `FromStr::Err` type. The error preserves
the original input and exposes both the rejected option family and every
accepted spelling:

```rust
use copybook::codec::{CodecOptionKind, ParseCodecOptionError, RecordFormat};
use std::str::FromStr;

fn parse_format(value: &str) -> Result<RecordFormat, ParseCodecOptionError> {
    RecordFormat::from_str(value)
}

let error = parse_format("blocked").expect_err("unsupported value");
assert_eq!(error.kind(), CodecOptionKind::RecordFormat);
assert_eq!(error.input(), "blocked");
assert_eq!(error.accepted_spellings(), &["fixed", "rdw"]);
```

The canonical types are available from `copybook::codec`,
`copybook_codec::options`, and the `copybook_codec` root. The compatibility
`copybook_options` package forwards the same contract. Display text remains
compatible with the former `String` errors.

These failures intentionally have no `CBK*` code. They represent textual
configuration-token conversion before a decode, encode, record-framing, or
infrastructure operation begins. Serde deserialization is a separate rejection
path and is not converted to `ParseCodecOptionError`.

'''
replace_once(
    Path("docs/reference/LIBRARY_API.md"),
    "\n## Codepage Notes\n",
    "\n" + library_section + "## Codepage Notes\n",
)
