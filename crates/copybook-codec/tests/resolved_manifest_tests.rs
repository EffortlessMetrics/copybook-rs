// SPDX-License-Identifier: AGPL-3.0-or-later
//! Resolved-schema manifest generation and verification (#1117).
//!
//! Generation binds bundle fingerprint, effective values with provenance,
//! flattened layout bounds, and matrix support status; verification rejects
//! tampered, versioned, and malformed documents.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::options::profile::{FramingKind, InterpretationProfile};
use copybook_codec::options::resolve::{OptionSource, Resolved, resolve_field};
use copybook_codec::resolved_manifest::{GenerateInputs, ManifestError, ResolvedManifest};
use copybook_core::dialect::Dialect;
use copybook_core::layout::resolve_layout;
use copybook_core::source_bundle::SourceBundle;
use copybook_core::{Schema, parse_copybook};

/// Copybook exercising alphanum, packed, zoned sign-separate, and level-88.
const MANIFEST_COPYBOOK: &str = concat!(
    "       01  REC.\n",
    "           05  NAME     PIC X(10).\n",
    "           05  AMOUNT   PIC 9(5) COMP-3.\n",
    "           05  BALANCE  PIC S9(5) SIGN TRAILING SEPARATE.\n",
    "           05  STATUS   PIC X.\n",
    "               88  ACTIVE   VALUE 'A'.\n",
);

fn resolved_str(value: &str, source: OptionSource) -> Resolved<String> {
    Resolved {
        value: value.to_owned(),
        source,
    }
}

fn test_inputs<'a>(bundle: &'a SourceBundle, schema: &'a Schema) -> GenerateInputs<'a> {
    GenerateInputs {
        bundle,
        encoding: resolved_str("cp037", OptionSource::Profile),
        dialect: Resolved {
            value: Dialect::Normative,
            source: OptionSource::Profile,
        },
        framing: resolved_str("fixed", OptionSource::Default),
        record_bound: Resolved {
            value: 32760,
            source: OptionSource::Default,
        },
        schema,
    }
}

fn generate_manifest() -> ResolvedManifest {
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    ResolvedManifest::generate(test_inputs(&bundle, &schema)).expect("manifest generates")
}

#[test]
fn manifest_binds_inputs_layout_and_support() {
    let manifest = generate_manifest();

    assert_eq!(manifest.schema_version, 1);
    assert_eq!(manifest.inputs.dialect.value, "normative");
    assert_eq!(manifest.inputs.dialect.provenance, "profile-selected");
    assert_eq!(manifest.inputs.encoding.source, "profile");
    assert_eq!(manifest.inputs.framing.source, "default");

    let name = manifest
        .fields
        .iter()
        .find(|field| field.path == "REC.NAME")
        .expect("NAME field present");
    assert_eq!((name.offset, name.len, name.end), (0, 10, 10));

    let encodings: Vec<&str> = manifest
        .numeric_details
        .iter()
        .map(|detail| detail.encoding.as_str())
        .collect();
    assert!(
        encodings.contains(&"packed-decimal"),
        "packed detail: {encodings:?}"
    );
    assert!(
        encodings.contains(&"zoned-sign-separate-trailing"),
        "zoned detail: {encodings:?}"
    );

    assert_eq!(manifest.condition_usages.len(), 1);
    assert!(manifest.condition_usages[0].path.contains("ACTIVE"));

    let support: Vec<(&str, &str)> = manifest
        .support
        .iter()
        .map(|entry| (entry.feature.as_str(), entry.status.as_str()))
        .collect();
    assert!(
        support.contains(&("level-88", "supported")),
        "support: {support:?}"
    );
    assert!(
        support.contains(&("sign-separate", "supported")),
        "support: {support:?}"
    );

    assert!(manifest.manifest_fingerprint.starts_with("sha256-v1:"));
    assert_eq!(manifest.manifest_fingerprint.len(), "sha256-v1:".len() + 64);
    assert_eq!(
        manifest.record_len,
        manifest.fields.iter().map(|f| f.end).max().unwrap_or(0)
    );
}

#[test]
fn manifest_round_trip_verifies() {
    let manifest = generate_manifest();
    let json = manifest.to_json().expect("manifest serializes");
    let parsed = ResolvedManifest::from_json(&json).expect("manifest verifies");
    assert_eq!(parsed.manifest_fingerprint, manifest.manifest_fingerprint);
    assert_eq!(parsed.fields.len(), manifest.fields.len());
}

#[test]
fn manifest_json_shape_matches_reference_schema() {
    // Guards drift between `to_json` output and `schemas/resolved-manifest.json`;
    // the schema file is reference-only (the Rust type is authoritative).
    let manifest = generate_manifest();
    let value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    let object = value.as_object().expect("top-level object");
    for key in [
        "schema_version",
        "stability_class",
        "fingerprint_algo",
        "inputs",
        "fields",
        "record_len",
        "lrecl",
        "numeric_details",
        "odo_details",
        "condition_usages",
        "support",
        "manifest_fingerprint",
    ] {
        assert!(object.contains_key(key), "missing key {key}");
    }
    assert_eq!(object.len(), 12, "unexpected keys: {object:?}");
}

#[test]
fn manifest_rejects_tampered_body() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    value["record_len"] = serde_json::json!(9999);
    let tampered = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&tampered).expect_err("tamper fails");
    assert!(
        matches!(err, ManifestError::FingerprintMismatch { .. }),
        "got {err}"
    );
}

#[test]
fn manifest_rejects_unknown_version() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    value["schema_version"] = serde_json::json!(999);
    // Version is checked before fingerprint verification, so the stale
    // fingerprint cannot mask the version error.
    let tampered = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&tampered).expect_err("version fails");
    assert!(
        matches!(
            err,
            ManifestError::UnsupportedManifestVersion { found: 999 }
        ),
        "got {err}"
    );
}

#[test]
fn manifest_rejects_malformed_json() {
    let err = ResolvedManifest::from_json(b"{not json").expect_err("malformed fails");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );
}

/// Reviewed TOML profile exercising every manifest resolution layer.
const JOURNEY_PROFILE: &str = concat!(
    "schema_version = 1\n",
    "[source]\n",
    "dialect = \"normative\"\n",
    "[framing]\n",
    "kind = \"rdw\"\n",
    "reserved_bytes = \"lenient\"\n",
    "[decode]\n",
    "codepage = \"cp037\"\n",
    "unmappable = \"error\"\n",
    "json_numbers = \"lossless\"\n",
    "[limits]\n",
    "maximum_record_length = 32760\n",
    "maximum_errors = 100\n",
);

fn framing_spelling(kind: FramingKind) -> String {
    match kind {
        FramingKind::Fixed => "fixed",
        FramingKind::Rdw => "rdw",
        FramingKind::Vb => "vb",
    }
    .to_owned()
}

#[test]
fn manifest_records_reviewed_profile_journey() {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");

    let dialect = resolve_field(
        "source.dialect",
        None,
        Some(Dialect::from(profile.source.dialect)),
        None,
        Dialect::default(),
    )
    .expect("dialect resolves");
    assert_eq!(dialect.source, OptionSource::Profile);
    let framing = resolve_field(
        "framing.kind",
        None,
        Some(framing_spelling(profile.framing.kind)),
        None,
        "fixed".to_owned(),
    )
    .expect("framing resolves");
    let encoding = resolve_field(
        "decode.codepage",
        None,
        Some(profile.decode.codepage.to_string()),
        None,
        "cp037".to_owned(),
    )
    .expect("encoding resolves");
    let record_bound = resolve_field(
        "limits.maximum_record_length",
        None,
        Some(profile.limits.maximum_record_length),
        None,
        32760,
    )
    .expect("record bound resolves");

    let manifest = ResolvedManifest::generate(GenerateInputs {
        bundle: &bundle,
        encoding,
        dialect,
        framing,
        record_bound,
        schema: &schema,
    })
    .expect("manifest generates");

    assert_eq!(manifest.inputs.dialect.value, "normative");
    assert_eq!(manifest.inputs.dialect.source, "profile");
    assert_eq!(manifest.inputs.framing.value, "rdw");
    assert_eq!(manifest.inputs.record_bound.value, 32760);
    assert_eq!(manifest.inputs.bundle_fingerprint, bundle.fingerprint());
}

#[test]
fn manifest_journey_flag_conflict_fails_resolution() {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let conflict = resolve_field(
        "framing.kind",
        Some("fixed".to_owned()),
        Some(framing_spelling(profile.framing.kind)),
        None,
        "fixed".to_owned(),
    )
    .expect_err("flag/profile disagreement fails");
    assert_eq!(conflict.field, "framing.kind");
}

#[test]
fn manifest_conflict_error_renders_both_dialects() {
    // No public bundle constructor declares a dialect today, so the
    // `DialectConflict` arm only triggers for stored bundles carrying a valid
    // declaration; the mapping itself is covered here through its message.
    let err = ManifestError::DialectConflict {
        declared: Dialect::Normative,
        selected: Dialect::ZeroTolerant,
    };
    let message = err.to_string();
    assert!(message.contains("Normative"), "got {message}");
    assert!(message.contains("ZeroTolerant"), "got {message}");
}
