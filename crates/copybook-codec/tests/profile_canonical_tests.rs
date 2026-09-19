// SPDX-License-Identifier: AGPL-3.0-or-later
//! Profile canonical bytes and fingerprint fixtures (#1113).
//!
//! Equivalent intent under hostile formatting (shuffled sections and keys,
//! comments, CRLF line endings) parses to one struct, renders byte-identical
//! canonical TOML matching the golden file, and fingerprints identically.
//! The golden fingerprint pins the identity the way golden hashes do.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::options::profile::InterpretationProfile;

fn fixture(name: &str) -> String {
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../fixtures/profiles")
        .join(name);
    std::fs::read_to_string(&path).expect("profile fixture reads")
}

/// Fingerprint of the golden canonical bytes (see `canonical-bytes.toml`).
const GOLDEN_FINGERPRINT: &str = "0334de43062dec12a4fa0ebc55c8f9ab450e04bd9315e6b8bb0876dff385e5c2";

#[test]
fn canonical_bytes_match_golden_fixture() {
    let golden = fixture("canonical-bytes.toml");
    // The golden file itself obeys the documented rendering rules.
    assert!(golden.ends_with('\n'));
    assert!(!golden.ends_with("\n\n"));
    assert!(!golden.contains("\r"));
    for line in golden.lines() {
        assert_eq!(line.trim_end(), line, "no trailing whitespace: {line:?}");
    }
    let profile = InterpretationProfile::parse(&fixture("base.toml")).expect("base parses");
    assert_eq!(profile.to_canonical_toml().expect("canonical"), golden);
}

#[test]
fn hostile_formatting_canonicalizes_identically() {
    let base = InterpretationProfile::parse(&fixture("base.toml")).expect("base parses");
    for name in ["shuffled.toml", "shuffled-crlf.toml"] {
        let text = fixture(name);
        let variant = InterpretationProfile::parse(&text).expect("variant parses");
        assert_eq!(variant, base, "equivalent intent: {name}");
        assert_eq!(
            variant.to_canonical_toml().expect("canonical"),
            base.to_canonical_toml().expect("canonical"),
            "byte-identical canonical form: {name}"
        );
        assert_eq!(
            variant.fingerprint().expect("fingerprints"),
            base.fingerprint().expect("fingerprints"),
            "identical identity: {name}"
        );
    }
}

#[test]
fn golden_fingerprint_pins_identity() {
    let profile = InterpretationProfile::parse(&fixture("base.toml")).expect("base parses");
    assert_eq!(
        profile.fingerprint().expect("fingerprints"),
        GOLDEN_FINGERPRINT
    );
}

#[test]
fn material_change_moves_fingerprint() {
    let base = InterpretationProfile::parse(&fixture("base.toml")).expect("base parses");
    let changed = fixture("base.toml").replace("maximum_errors = 100", "maximum_errors = 99");
    let other = InterpretationProfile::parse(&changed).expect("changed parses");
    assert_ne!(
        other.fingerprint().expect("fingerprints"),
        base.fingerprint().expect("fingerprints")
    );
}
