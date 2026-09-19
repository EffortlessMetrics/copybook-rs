// SPDX-License-Identifier: AGPL-3.0-or-later
//! Source bundle identity through the public core API (#1116).
//!
//! Bundles built from identical bytes under different local paths share
//! identity; changed bytes change it. Bundle bytes parse through the
//! supported schema API, and the three identities (raw source, schema,
//! bundle) stay distinct facts.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_core::{SourceBundle, logical_id_for_path, parse_copybook, resolve_effective_dialect};
use std::path::Path;

const SIMPLE_CPY: &[u8] = b"       01  REC.\n           05  NAME     PIC X(10).\n";

#[test]
fn cobol_bundle_bytes_parse_through_supported_api() {
    let bundle = SourceBundle::single("simple.cpy", SIMPLE_CPY).expect("bundle");
    let text = std::str::from_utf8(SIMPLE_CPY).expect("UTF-8");
    let schema = parse_copybook(text).expect("schema");
    assert!(!schema.fingerprint.is_empty());
    assert_ne!(schema.fingerprint, bundle.source_fingerprint());
    assert_ne!(schema.fingerprint, bundle.fingerprint());
}

#[test]
fn cobol_bundle_identity_path_independent_across_directories() {
    let left = SourceBundle::single(
        logical_id_for_path(Path::new("/work/a/simple.cpy")),
        SIMPLE_CPY,
    )
    .expect("bundle");
    let right = SourceBundle::single(
        logical_id_for_path(Path::new("/home/other/b/simple.cpy")),
        SIMPLE_CPY,
    )
    .expect("bundle");
    assert_eq!(left.fingerprint(), right.fingerprint());
    assert_eq!(left.source_fingerprint(), right.source_fingerprint());
}

#[test]
fn cobol_bundle_undeclared_with_profile_selection_agrees() {
    let effective = resolve_effective_dialect(None, Some(copybook_core::Dialect::ZeroTolerant))
        .expect("agreement");
    assert_eq!(effective.dialect, copybook_core::Dialect::ZeroTolerant);
}

#[test]
fn cobol_bundle_canonical_evidence_stable_json() {
    let bundle = SourceBundle::single("simple.cpy", SIMPLE_CPY).expect("bundle");
    let first = bundle.canonical_json().expect("rendering");
    let second = SourceBundle::single("simple.cpy", SIMPLE_CPY)
        .expect("bundle")
        .canonical_json()
        .expect("rendering");
    assert_eq!(first, second);
    assert!(first.contains("\"schema_version\": 1"));
    assert!(first.contains("\"stability_class\": \"beta\""));
    assert!(first.contains("\"include_support\": \"Unsupported\""));
}
