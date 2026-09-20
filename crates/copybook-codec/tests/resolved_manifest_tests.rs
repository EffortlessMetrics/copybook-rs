// SPDX-License-Identifier: AGPL-3.0-or-later
//! Resolved-schema manifest generation and verification (#1117).
//!
//! Generation binds bundle fingerprint, effective values with provenance,
//! flattened layout bounds, and matrix support status; verification rejects
//! tampered, versioned, and malformed documents.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::options::profile::{FramingKind, InterpretationProfile};
use copybook_codec::options::resolve::{OptionSource, Resolved, resolve_field};
use copybook_codec::resolved_manifest::{
    GenerateInputs, MANIFEST_SOURCE_SPANS, ManifestError, ManifestTool, ResolvedManifest,
};
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

fn test_tool() -> ManifestTool {
    ManifestTool {
        name: "copybook-test".to_owned(),
        version: "0.0.0".to_owned(),
    }
}

fn test_inputs<'a>(
    bundle: &'a SourceBundle,
    schema: &'a Schema,
    profile: Option<&'a InterpretationProfile>,
) -> GenerateInputs<'a> {
    GenerateInputs {
        bundle,
        profile,
        tool: test_tool(),
        encoding: resolved_str("cp037", OptionSource::Profile),
        dialect: Resolved {
            value: Dialect::Normative,
            source: OptionSource::Profile,
        },
        framing: resolved_str("fixed", OptionSource::Default),
        record_bound: Some(Resolved {
            value: 32760,
            source: OptionSource::Default,
        }),
        schema,
    }
}

fn generate_manifest() -> ResolvedManifest {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates")
}

#[test]
fn cobol_manifest_binds_inputs_layout_and_support() {
    let manifest = generate_manifest();

    assert_eq!(manifest.schema_version, 2);
    assert_eq!(manifest.stability_class, "beta");
    assert_eq!(manifest.inputs.bundle.schema_version, 1);
    assert_eq!(manifest.inputs.bundle.root_unit, "REC");
    assert!(!manifest.inputs.bundle.fingerprint.is_empty());
    let profile = manifest.inputs.profile.as_ref().expect("profile identity");
    assert_eq!(profile.schema_version, 2);
    assert_eq!(profile.fingerprint.len(), 64);
    assert_eq!(manifest.inputs.tool.name, "copybook-test");
    assert_eq!(manifest.inputs.tool.version, "0.0.0");
    assert_eq!(manifest.schema_fingerprint.len(), 64);
    assert_eq!(manifest.record_len_min, Some(manifest.record_len));
    assert_eq!(manifest.source_spans, MANIFEST_SOURCE_SPANS);
    assert!(manifest.renames.is_empty());
    assert!(manifest.redefines_groups.is_empty());
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

    let fingerprint = manifest.fingerprint();
    assert!(fingerprint.starts_with("sha256-v1:"));
    assert_eq!(fingerprint.len(), "sha256-v1:".len() + 64);
    assert_eq!(
        manifest.record_len,
        manifest.fields.iter().map(|f| f.end).max().unwrap_or(0)
    );
}

#[test]
fn cobol_manifest_round_trip_verifies() {
    let manifest = generate_manifest();
    let json = manifest.to_json().expect("manifest serializes");
    let parsed = ResolvedManifest::from_json(&json).expect("manifest verifies");
    assert_eq!(parsed.fingerprint(), manifest.fingerprint());
    assert_eq!(parsed, manifest);
}

#[test]
fn cobol_manifest_json_shape_matches_reference_schema() {
    // Guards drift between `to_json` output and `schemas/resolved-manifest.json`;
    // the schema file is reference-only (the Rust type is authoritative).
    let manifest = generate_manifest();
    let value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    let object = value.as_object().expect("top-level object");
    // Read the reference schema so schema-only drift fails this guard.
    let schema_path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../schemas/resolved-manifest.json"
    );
    let schema_text = std::fs::read_to_string(schema_path).expect("reference schema reads");
    let schema: serde_json::Value =
        serde_json::from_str(&schema_text).expect("reference schema parses");
    let required: Vec<&str> = schema["required"]
        .as_array()
        .expect("schema has required keys")
        .iter()
        .map(|key| key.as_str().expect("required key is a string"))
        .collect();
    for key in &required {
        assert!(object.contains_key(*key), "missing key {key}");
    }
    assert_eq!(object.len(), required.len(), "unexpected keys: {object:?}");
}

#[test]
fn cobol_manifest_uncapped_run_records_null_bound() {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let mut inputs = test_inputs(&bundle, &schema, Some(&profile));
    inputs.record_bound = None;
    let manifest = ResolvedManifest::generate(inputs).expect("manifest generates");
    assert!(manifest.inputs.record_bound.is_none());
    let json = manifest.to_json().expect("serializes");
    let round_trip = ResolvedManifest::from_json(&json).expect("verifies");
    assert!(round_trip.inputs.record_bound.is_none());
}

#[test]
fn cobol_manifest_rejects_tampered_body() {
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
fn cobol_manifest_rejects_unknown_version() {
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
fn cobol_manifest_rejects_unknown_keys() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    value["injected_property"] = serde_json::json!("not part of the contract");
    let injected = serde_json::to_vec(&value).expect("re-serializes");
    // Unknown properties feed the fingerprint, so injection without
    // re-signing fails verification rather than parsing.
    let err = ResolvedManifest::from_json(&injected).expect_err("unknown keys fail");
    assert!(
        matches!(err, ManifestError::FingerprintMismatch { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_rejects_null_occurs_before_verification() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    value["fields"][0]["occurs"] = serde_json::Value::Null;
    // The stale fingerprint is intact, so only the null guard reports: an
    // explicit null would deserialize away while staying in the verified
    // body, splitting the reported fingerprint from the document's own.
    let nulled = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&nulled).expect_err("null occurs fails");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_rejects_oversized_input() {
    use copybook_codec::resolved_manifest::MAX_MANIFEST_BYTES;
    let oversized = vec![b' '; MAX_MANIFEST_BYTES + 1];
    let err = ResolvedManifest::from_json(&oversized).expect_err("oversized fails");
    assert!(
        matches!(err, ManifestError::ManifestTooLarge { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_rejects_unknown_stability_class() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    value["stability_class"] = serde_json::json!("stable");
    // Contract identity is checked before fingerprint verification.
    let tampered = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&tampered).expect_err("class fails");
    assert!(
        matches!(err, ManifestError::UnsupportedStabilityClass { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_rejects_unknown_fingerprint_algo() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    value["fingerprint_algo"] = serde_json::json!("md5-v0");
    let tampered = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&tampered).expect_err("algo fails");
    assert!(
        matches!(err, ManifestError::UnsupportedFingerprintAlgo { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_verifies_despite_reordered_keys() {
    let manifest = generate_manifest();
    let value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    // Reverse every object's key order: canonical bytes must be unaffected.
    fn reversed(value: &serde_json::Value) -> serde_json::Value {
        match value {
            serde_json::Value::Object(object) => {
                let mut out = serde_json::Map::with_capacity(object.len());
                for (key, item) in object.iter().rev() {
                    out.insert(key.clone(), reversed(item));
                }
                serde_json::Value::Object(out)
            }
            serde_json::Value::Array(items) => {
                serde_json::Value::Array(items.iter().map(reversed).collect())
            }
            _ => value.clone(),
        }
    }
    let reordered = serde_json::to_vec(&reversed(&value)).expect("re-serializes");
    let parsed = ResolvedManifest::from_json(&reordered).expect("reordered verifies");
    assert_eq!(parsed, manifest);
}

#[test]
fn cobol_manifest_emitted_wire_validates_against_reference_schema() {
    let schema_path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../schemas/resolved-manifest.json"
    );
    let schema_text = std::fs::read_to_string(schema_path).expect("reference schema reads");
    let schema: serde_json::Value =
        serde_json::from_str(&schema_text).expect("reference schema parses");
    let validator = jsonschema::validator_for(&schema).expect("schema compiles");

    // Positive: capped and uncapped (null record bound) documents validate.
    let manifest = generate_manifest();
    let capped: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    assert!(validator.is_valid(&capped), "capped document validates");

    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let mut inputs = test_inputs(&bundle, &schema, Some(&profile));
    inputs.record_bound = None;
    let uncapped = ResolvedManifest::generate(inputs).expect("uncapped manifest generates");
    let null_bound: serde_json::Value =
        serde_json::from_slice(&uncapped.to_json().expect("serializes")).expect("json parses");
    assert_eq!(
        null_bound["inputs"]["record_bound"],
        serde_json::Value::Null
    );
    assert!(
        validator.is_valid(&null_bound),
        "null-bound document validates"
    );

    // Negative: mistyped and truncated documents fail.
    let mut mistyped = capped.clone();
    mistyped["record_len"] = serde_json::json!("sixty-one");
    assert!(!validator.is_valid(&mistyped), "mistyped document fails");
    let mut missing = capped.clone();
    missing
        .as_object_mut()
        .expect("document object")
        .remove("manifest_fingerprint");
    assert!(!validator.is_valid(&missing), "truncated document fails");
}

#[test]
fn cobol_manifest_rejects_malformed_json() {
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
fn cobol_manifest_records_reviewed_profile_journey() {
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
        "representation.codepage",
        None,
        Some(profile.representation.codepage.to_string()),
        None,
        "cp037".to_owned(),
    )
    .expect("encoding resolves");
    let record_bound = Some(
        resolve_field(
            "limits.maximum_record_length",
            None,
            Some(profile.limits.maximum_record_length),
            None,
            32760,
        )
        .expect("record bound resolves"),
    );

    let manifest = ResolvedManifest::generate(GenerateInputs {
        bundle: &bundle,
        profile: Some(&profile),
        tool: test_tool(),
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
    let bound = manifest
        .inputs
        .record_bound
        .as_ref()
        .expect("bound present");
    assert_eq!(bound.value, 32760);
    assert_eq!(bound.source, "profile");
    assert_eq!(manifest.inputs.bundle.fingerprint, bundle.fingerprint());
    let identity = manifest.inputs.profile.as_ref().expect("profile identity");
    assert_eq!(identity.schema_version, profile.schema_version);
    assert_eq!(
        identity.fingerprint,
        profile.fingerprint().expect("profile fingerprints")
    );
}

#[test]
fn cobol_manifest_journey_flag_conflict_fails_resolution() {
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
fn cobol_manifest_conflict_error_renders_both_dialects() {
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

#[test]
fn cobol_manifest_profileless_run_records_no_profile_identity() {
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, None))
        .expect("manifest generates");
    assert!(manifest.inputs.profile.is_none());
    let json = manifest.to_json().expect("serializes");
    let round_trip = ResolvedManifest::from_json(&json).expect("verifies");
    assert!(round_trip.inputs.profile.is_none());
    assert_eq!(round_trip.inputs.tool.name, "copybook-test");
}

#[test]
fn cobol_manifest_variable_layout_reports_no_static_minimum() {
    // NOTE: a single space separates the field name from OCCURS; multiple
    // spaces there currently misparse (pre-existing parser quirk, out of
    // scope for the manifest contract).
    const ODO_COPYBOOK: &str = concat!(
        "       01 ODO-REC.\n",
        "           05 COUNTER PIC 9(3).\n",
        "           05 CELLS OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).\n",
    );
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single("ODO-REC", ODO_COPYBOOK.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(ODO_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert!(!manifest.odo_details.is_empty());
    assert_eq!(manifest.record_len_min, None);
    let json = manifest.to_json().expect("serializes");
    ResolvedManifest::from_json(&json).expect("verifies");
}

#[test]
fn cobol_manifest_records_renames_aliases() {
    const RENAMES_COPYBOOK: &str = concat!(
        "       01  RECORD-A.\n",
        "           05  FIELD-1  PIC X(10).\n",
        "           05  FIELD-2  PIC 9(5).\n",
        "           05  FIELD-3  PIC X(2).\n",
        "           66  ALIAS-A  RENAMES FIELD-1 THRU FIELD-3.\n",
    );
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle =
        SourceBundle::single("RECORD-A", RENAMES_COPYBOOK.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(RENAMES_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert_eq!(manifest.renames.len(), 1);
    let alias = &manifest.renames[0];
    assert!(alias.path.contains("ALIAS-A"), "got {}", alias.path);
    assert_eq!((alias.offset, alias.length), (0, 17));
    assert_eq!(alias.members.len(), 3);
}

#[test]
fn cobol_manifest_groups_redefines_storage_views() {
    const REDEFINES_COPYBOOK: &str = concat!(
        "       01  ACCT-REC.\n",
        "           05  PRIMARY    PIC X(6).\n",
        "           05  SECONDARY  REDEFINES PRIMARY PIC X(6).\n",
    );
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle =
        SourceBundle::single("ACCT-REC", REDEFINES_COPYBOOK.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(REDEFINES_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert_eq!(manifest.redefines_groups.len(), 1);
    let group = &manifest.redefines_groups[0];
    // Storage and every view join to real field paths: the clause's
    // unqualified target spelling never leaks into relations.
    assert_eq!(group.storage, "ACCT-REC.PRIMARY");
    assert_eq!(group.views, vec!["ACCT-REC.PRIMARY", "ACCT-REC.SECONDARY"]);
    for view in &group.views {
        assert!(
            manifest.fields.iter().any(|field| &field.path == view),
            "view joins to a field: {view}"
        );
    }
    let secondary = manifest
        .fields
        .iter()
        .find(|field| field.path == "ACCT-REC.SECONDARY")
        .expect("SECONDARY present");
    assert_eq!(secondary.redefines.as_deref(), Some("ACCT-REC.PRIMARY"));
}

#[test]
fn cobol_manifest_chained_redefines_collapse_to_one_storage() {
    const CHAIN_COPYBOOK: &str = concat!(
        "       01 CHAIN-REC.\n",
        "           05 BASE PIC X(4).\n",
        "           05 MID REDEFINES BASE PIC X(4).\n",
        "           05 TOP REDEFINES MID PIC X(4).\n",
    );
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle =
        SourceBundle::single("CHAIN-REC", CHAIN_COPYBOOK.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(CHAIN_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert_eq!(manifest.redefines_groups.len(), 1);
    let group = &manifest.redefines_groups[0];
    assert_eq!(group.storage, "CHAIN-REC.BASE");
    assert_eq!(
        group.views,
        vec!["CHAIN-REC.BASE", "CHAIN-REC.MID", "CHAIN-REC.TOP"]
    );
}

#[test]
fn cobol_manifest_rejects_previous_generation_version() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    // A version-1-shaped document reports its generation, even with a stale
    // fingerprint: contract identity reads before verification.
    value["schema_version"] = serde_json::json!(1);
    let downgraded = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&downgraded).expect_err("v1 fails");
    assert!(
        matches!(err, ManifestError::UnsupportedManifestVersion { found: 1 }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_schema_fingerprint_matches_layout_identity() {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let mut schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    // Layout resolution mutates offsets without refreshing the stored schema
    // fingerprint, so the manifest must track the resolved layout, not the
    // stale stored value.
    let pre_layout = schema.fingerprint.clone();
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert_ne!(manifest.schema_fingerprint, pre_layout);
    assert_eq!(manifest.schema_fingerprint.len(), 64);
    assert!(
        manifest
            .schema_fingerprint
            .chars()
            .all(|c| c.is_ascii_hexdigit()),
        "got {}",
        manifest.schema_fingerprint
    );
    // Deterministic: the same resolved schema re-fingerprints identically.
    let again = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert_eq!(again.schema_fingerprint, manifest.schema_fingerprint);
}

#[test]
fn cobol_manifest_distinct_layouts_fingerprint_distinctly() {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let wider = MANIFEST_COPYBOOK.replace("PIC X(10)", "PIC X(11)");
    let mut first_schema = parse_copybook(MANIFEST_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut first_schema, Dialect::Normative).expect("layout resolves");
    let mut second_schema = parse_copybook(&wider).expect("copybook parses");
    resolve_layout(&mut second_schema, Dialect::Normative).expect("layout resolves");
    let bundle = SourceBundle::single("REC", MANIFEST_COPYBOOK.as_bytes())
        .expect("single-unit bundle builds");
    let first = ResolvedManifest::generate(test_inputs(&bundle, &first_schema, Some(&profile)))
        .expect("manifest generates");
    let second = ResolvedManifest::generate(test_inputs(&bundle, &second_schema, Some(&profile)))
        .expect("manifest generates");
    assert_ne!(
        first.schema_fingerprint, second.schema_fingerprint,
        "one wider field changes the layout identity"
    );
    assert_ne!(first.record_len, second.record_len);
}

#[test]
fn cobol_manifest_mistyped_identity_is_malformed() {
    let manifest = generate_manifest();
    let mut value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    // Identity reads before verification, so even a stale fingerprint reports
    // the wire shape rather than a version or digest error.
    value["schema_version"] = serde_json::json!("2");
    let mistyped = serde_json::to_vec(&value).expect("re-serializes");
    let err = ResolvedManifest::from_json(&mistyped).expect_err("string version fails");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );

    let mut missing = value.clone();
    missing
        .as_object_mut()
        .expect("document is an object")
        .remove("schema_version");
    let missing_bytes = serde_json::to_vec(&missing).expect("re-serializes");
    let err = ResolvedManifest::from_json(&missing_bytes).expect_err("missing version fails");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_rejects_non_hex_identity_fingerprints() {
    let mut manifest = generate_manifest();
    manifest.schema_fingerprint = "not-a-digest".to_owned();
    let json = manifest.to_json().expect("serializes under fresh digest");
    let err = ResolvedManifest::from_json(&json).expect_err("non-hex digest fails");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );

    let mut manifest = generate_manifest();
    manifest.inputs.bundle.fingerprint = "xyz".to_owned();
    let json = manifest.to_json().expect("serializes under fresh digest");
    let err = ResolvedManifest::from_json(&json).expect_err("non-hex bundle fails");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );
}

#[test]
fn cobol_manifest_rejects_empty_renames_and_misordered_storage() {
    const RENAMES_COPYBOOK: &str = concat!(
        "       01  RECORD-A.\n",
        "           05  FIELD-1  PIC X(10).\n",
        "           05  FIELD-2  PIC 9(5).\n",
        "           66  ALIAS-A  RENAMES FIELD-1 THRU FIELD-2.\n",
    );
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle =
        SourceBundle::single("RECORD-A", RENAMES_COPYBOOK.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(RENAMES_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let mut manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    assert_eq!(manifest.renames.len(), 1);
    manifest.renames[0].members.clear();
    let json = manifest.to_json().expect("serializes under fresh digest");
    let err = ResolvedManifest::from_json(&json).expect_err("empty members fail");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );

    const REDEFINES_COPYBOOK: &str = concat!(
        "       01  ACCT-REC.\n",
        "           05  PRIMARY    PIC X(6).\n",
        "           05  SECONDARY  REDEFINES PRIMARY PIC X(6).\n",
    );
    let bundle =
        SourceBundle::single("ACCT-REC", REDEFINES_COPYBOOK.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(REDEFINES_COPYBOOK).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    let mut manifest = ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates");
    manifest.redefines_groups[0].views.reverse();
    let json = manifest.to_json().expect("serializes under fresh digest");
    let err = ResolvedManifest::from_json(&json).expect_err("misordered views fail");
    assert!(
        matches!(err, ManifestError::MalformedManifest { .. }),
        "got {err}"
    );
}

/// OCCURS repetition bounds ride the `fields` wire items (#1122).
const OCCURS_COPYBOOK: &str = concat!(
    "       01 TBL-REC.\n",
    "           05 CELLS PIC X(4) OCCURS 3 TIMES.\n",
    "           05 HOWMANY PIC 9(2).\n",
    "           05 AMOUNTS OCCURS 1 TO 5 TIMES DEPENDING ON HOWMANY PIC X(3).\n",
);

fn generate_manifest_for(copybook: &str, root: &str) -> ResolvedManifest {
    let profile = InterpretationProfile::parse(JOURNEY_PROFILE).expect("profile parses");
    let bundle = SourceBundle::single(root, copybook.as_bytes()).expect("bundle builds");
    let mut schema = parse_copybook(copybook).expect("copybook parses");
    resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
    ResolvedManifest::generate(test_inputs(&bundle, &schema, Some(&profile)))
        .expect("manifest generates")
}

#[test]
fn cobol_manifest_fields_carry_occurs_bounds() {
    let manifest = generate_manifest_for(OCCURS_COPYBOOK, "TBL-REC");
    let cells = manifest
        .fields
        .iter()
        .find(|field| field.path == "TBL-REC.CELLS")
        .expect("fixed table present");
    let fixed = cells.occurs.as_ref().expect("fixed occurs present");
    assert_eq!(fixed.kind, "fixed");
    assert_eq!(fixed.count, 3);
    assert_eq!(fixed.min_count, 3);
    assert!(fixed.counter_path.is_none());

    let amounts = manifest
        .fields
        .iter()
        .find(|field| field.path == "TBL-REC.AMOUNTS")
        .expect("odo table present");
    let odo = amounts.occurs.as_ref().expect("odo occurs present");
    assert_eq!(odo.kind, "odo");
    assert_eq!(odo.count, 5);
    assert_eq!(odo.min_count, 1);
    assert_eq!(odo.counter_path.as_deref(), Some("HOWMANY"));

    let scalar = manifest
        .fields
        .iter()
        .find(|field| field.path == "TBL-REC.HOWMANY")
        .expect("scalar present");
    assert!(scalar.occurs.is_none());
}

#[test]
fn cobol_manifest_omits_occurs_for_scalar_layouts() {
    // Scalar-only manifests must re-serialize without the optional key so
    // pre-existing fingerprints stay stable.
    let manifest = generate_manifest();
    let value: serde_json::Value =
        serde_json::from_slice(&manifest.to_json().expect("serializes")).expect("json parses");
    for field in value["fields"].as_array().expect("fields array") {
        assert!(
            field.get("occurs").is_none(),
            "scalar field carries no occurs key: {field}"
        );
    }
}
