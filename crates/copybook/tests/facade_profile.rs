// SPDX-License-Identifier: AGPL-3.0-or-later
//! Clean facade consumer for the reviewed profile path (#1113).
//!
//! Uses only the preferred facade spelling
//! (`copybook::codec::options::profile`) to parse, validate, canonicalize,
//! and fingerprint: the external consumer never touches granular crate
//! paths.

use copybook::codec::options::profile::InterpretationProfile;

const PROFILE: &str = "schema_version = 1\n[source]\ndialect = \"normative\"\n[framing]\nkind = \"fixed\"\nreserved_bytes = \"lenient\"\n[decode]\ncodepage = \"cp037\"\nunmappable = \"error\"\njson_numbers = \"lossless\"\n[limits]\nmaximum_record_length = 32760\nmaximum_errors = 100\n";

/// The preferred path parses, validates, canonicalizes, and fingerprints.
/// `PROFILE` is a version 1 document: the clean consumer migrates without
/// touching granular crate paths.
#[test]
fn facade_profile_preferred_path_end_to_end() {
    let profile = InterpretationProfile::parse(PROFILE).expect("profile parses");
    assert_eq!(profile.schema_version, 2);
    let canonical = profile.to_canonical_toml().expect("canonical renders");
    assert!(canonical.ends_with('\n'));
    let fingerprint = profile.fingerprint().expect("profile fingerprints");
    assert_eq!(fingerprint.len(), 64);
    // Canonical bytes re-parse to the identical profile: the preferred path
    // round-trips without loss.
    let again = InterpretationProfile::parse(&canonical).expect("canonical parses");
    assert_eq!(again, profile);
}

/// Misspellings fail through the preferred path, never silently.
#[test]
fn facade_profile_rejects_unknown_keys() {
    let mistyped = PROFILE.replace("maximum_errors = 100", "maximum_erors = 100");
    InterpretationProfile::parse(&mistyped).expect_err("unknown key rejected");
}
