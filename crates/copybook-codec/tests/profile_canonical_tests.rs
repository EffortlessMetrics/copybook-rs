// SPDX-License-Identifier: AGPL-3.0-or-later
//! Profile canonical bytes and fingerprint fixtures (#1113).
//!
//! Equivalent intent under hostile formatting (shuffled sections and keys,
//! comments, CRLF line endings) parses to one struct, renders byte-identical
//! canonical TOML matching the golden file, and fingerprints identically.
//! The golden fingerprint pins the identity the way golden hashes do.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::options::profile::{InterpretationProfile, MAX_PROFILE_BYTES, ProfileError};

fn fixture(name: &str) -> String {
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../fixtures/profiles")
        .join(name);
    std::fs::read_to_string(&path).expect("profile fixture reads")
}

/// Fingerprint of the golden canonical bytes (see `canonical-bytes.toml`).
/// Re-pinned for schema version 2 (#1120): the wire split moves bytes by
/// design, so the identity moves with them.
const GOLDEN_FINGERPRINT: &str = "c3eacd23cd4699b54abd93309b5c9202bbfc0a4e9733af0ce65a6aa80cd70195";

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

#[test]
fn oversize_document_rejects_before_parsing() {
    // A document past MAX_PROFILE_BYTES is refused with its size attached,
    // without TOML parsing ever running: profiles are small reviewed
    // documents, never data payloads.
    let mut oversized = fixture("base.toml");
    let padding = "# pad\n".repeat(256 * 1024);
    oversized.push_str(&padding);
    assert!(
        u64::try_from(oversized.len()).unwrap_or(0) > MAX_PROFILE_BYTES,
        "test document must exceed the bound"
    );
    let result = InterpretationProfile::parse(&oversized);
    assert!(
        matches!(
            result,
            Err(ProfileError::ProfileTooLarge { found })
                if found == u64::try_from(oversized.len()).unwrap_or(0)
        ),
        "expected ProfileTooLarge with the document size, got {result:?}"
    );
}

#[test]
fn wire_documents_validate_against_reference_schema() {
    let schema_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../schemas/interpretation-profile.json");
    let schema_text = std::fs::read_to_string(&schema_path).expect("reference schema reads");
    let schema: serde_json::Value =
        serde_json::from_str(&schema_text).expect("reference schema parses");
    let validator = jsonschema::validator_for(&schema).expect("schema compiles");

    // Positive: every canonical fixture validates, including hostile
    // formatting variants (they parse to identical intent).
    for name in [
        "base.toml",
        "canonical-bytes.toml",
        "shuffled.toml",
        "shuffled-crlf.toml",
    ] {
        let profile = InterpretationProfile::parse(&fixture(name)).expect("fixture parses");
        let document: serde_json::Value =
            serde_json::to_value(&profile).expect("profile serializes");
        assert!(
            validator.is_valid(&document),
            "fixture {name} validates against the reference schema"
        );
    }

    // Negative: unknown keys, wrong enums, missing sections, and
    // out-of-range bounds all fail.
    let profile = InterpretationProfile::parse(&fixture("base.toml")).expect("base parses");
    let valid: serde_json::Value = serde_json::to_value(&profile).expect("serializes");
    let mut unknown = valid.clone();
    unknown["framing"]["watermark"] = serde_json::json!("sneaky");
    assert!(!validator.is_valid(&unknown), "unknown key fails");
    let mut bad_enum = valid.clone();
    bad_enum["source"]["dialect"] = serde_json::json!("sometimes");
    assert!(!validator.is_valid(&bad_enum), "unknown dialect fails");
    let mut missing = valid.clone();
    missing
        .as_object_mut()
        .expect("document object")
        .remove("limits");
    assert!(!validator.is_valid(&missing), "missing section fails");
    let mut over_bound = valid.clone();
    over_bound["limits"]["maximum_record_length"] = serde_json::json!(16_777_217u64);
    assert!(
        !validator.is_valid(&over_bound),
        "over-bound record length fails"
    );
    let mut zero_bound = valid;
    zero_bound["limits"]["maximum_record_length"] = serde_json::json!(0u64);
    assert!(!validator.is_valid(&zero_bound), "zero record length fails");
}
