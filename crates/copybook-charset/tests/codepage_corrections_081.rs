// SPDX-License-Identifier: AGPL-3.0-or-later
//! 0.8.1 codepage corrections (#998 CP1140, #999 CP273/CP1047).
//!
//! Independent references: ICU ibm-1140_P100-1997, ibm-273_P100-1995,
//! ibm-1047_P100-1995 UCM mappings at revision
//! `61607c27732906d36c5bd4d23ecc092f89f53a2b`, cross-checked with CPython
//! `cp1140`/`cp273`/`cp1047` codecs and glibc iconv. These tests assert exact
//! bytes and codepoints in both directions, so a mutually consistent but
//! externally wrong table cannot pass.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use copybook_error::ErrorCode;

// ---------------------------------------------------------------------------
// #998: CP1140 Euro lives at 0x9F; 0xFF is control U+009F.
// ---------------------------------------------------------------------------

#[test]
fn cp1140_decode_9f_is_euro() {
    let decoded = ebcdic_to_utf8(&[0x9F], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "€", "CP1140: byte 0x9F must decode to U+20AC");
}

#[test]
fn cp1140_decode_ff_is_control() {
    let decoded = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Replace).unwrap();
    assert_eq!(decoded, "\u{9f}", "CP1140: byte 0xFF must decode to U+009F");
}

#[test]
fn cp1140_encode_euro_is_9f() {
    let encoded = utf8_to_ebcdic("€", Codepage::CP1140).unwrap();
    assert_eq!(encoded, vec![0x9F], "CP1140: € must encode to 0x9F");
}

#[test]
fn cp1140_encode_currency_sign_rejected() {
    let err = utf8_to_ebcdic("¤", Codepage::CP1140).unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        "CP1140: ¤ has no mapping and must be rejected under the strict encoder"
    );
}

#[test]
fn cp037_contrast_9f_is_currency_sign() {
    let decoded = ebcdic_to_utf8(&[0x9F], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "¤", "CP037: byte 0x9F must still decode to U+00A4");
    let encoded = utf8_to_ebcdic("¤", Codepage::CP037).unwrap();
    assert_eq!(encoded, vec![0x9F], "CP037: ¤ must still encode to 0x9F");
}

// ---------------------------------------------------------------------------
// #999: CP273 0x59 is ~ and 0xA1 is ß (currently swapped).
// ---------------------------------------------------------------------------

#[test]
fn cp273_decode_tilde_and_eszett() {
    let decoded = ebcdic_to_utf8(&[0x59, 0xA1], Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "~ß", "CP273: bytes 59 A1 must decode to ~ß");
}

#[test]
fn cp273_encode_tilde_and_eszett() {
    let encoded = utf8_to_ebcdic("~ß", Codepage::CP273).unwrap();
    assert_eq!(encoded, vec![0x59, 0xA1], "CP273: ~ß must encode to 59 A1");
}

// ---------------------------------------------------------------------------
// #999: CP1047 0x5F is ^ and 0xB0 is ¬ (currently swapped).
// ---------------------------------------------------------------------------

#[test]
fn cp1047_decode_caret_and_not_sign() {
    let decoded = ebcdic_to_utf8(&[0x5F, 0xB0], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "^¬", "CP1047: bytes 5F B0 must decode to ^¬");
}

#[test]
fn cp1047_encode_caret_and_not_sign() {
    let encoded = utf8_to_ebcdic("^¬", Codepage::CP1047).unwrap();
    assert_eq!(encoded, vec![0x5F, 0xB0], "CP1047: ^¬ must encode to 5F B0");
}
