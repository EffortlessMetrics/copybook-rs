// SPDX-License-Identifier: AGPL-3.0-or-later
//! Generated scenario authority for `support --advise` (#978).
//!
//! DO NOT EDIT. Source: `docs/evidence/scenario-ledger.toml`.
//! Regenerate: `cargo run -p xtask -- advise sync-projection`.
//! Freshness is enforced by `docs verify-all` (`advise-projection`).
//! Compiled in: no runtime file I/O, registry-install safe.

use crate::advise::{AffectedLayer, AssessmentStatus};

/// One scenario authority row: applicability, outcome, and proof.
/// `layers` carries only schema-renderable layers; `negative`-layer
/// proof still rides in `evidence`.
pub struct LedgerProjection {
    /// Ledger scenario ID.
    pub id: &'static str,
    /// Record formats this row evidences (`fixed`, `rdw`, `vb` subset).
    pub formats: &'static [&'static str],
    /// Codepages this row evidences (`all` or an explicit subset).
    pub codepages: &'static [&'static str],
    /// Row outcome mapped from the ledger support status.
    pub status: AssessmentStatus,
    /// Row stability class verbatim (`stable`, `beta`).
    pub stability: &'static str,
    /// Renderable affected layers in ledger order.
    pub layers: &'static [AffectedLayer],
    /// Direct evidence refs (`path::test`) in ledger layer order.
    pub evidence: &'static [&'static str],
    /// Row limitations/remediation verbatim (empty when none stated).
    pub limitations: &'static str,
}

/// Full ledger authority, sorted by scenario ID for determinism.
pub const LEDGER_PROJECTION: &[LedgerProjection] = &[
    LedgerProjection {
        id: "format.vb.basic",
        formats: &["vb"],
        codepages: &["ASCII"],
        status: AssessmentStatus::Beta,
        stability: "beta",
        layers: &[AffectedLayer::Decode],
        evidence: &[
            "crates/copybook-codec/tests/encoding_vb_blocks.rs::encoding_vb_decodes_records_across_blocks",
            "crates/copybook-codec/tests/encoding_vb_blocks.rs::encoding_vb_jsonl_encode_decode_round_trip",
            "crates/copybook-codec/tests/encoding_vb_blocks.rs::encoding_vb_jsonl_encode_decode_round_trip",
            "crates/copybook-codec/tests/encoding_vb_blocks.rs::encoding_vb_jsonl_encode_decode_round_trip",
            "crates/copybook-codec/tests/encoding_vb_blocks.rs::encoding_vb_truncated_block_fails_strict",
        ],
        limitations: "VB/BDW is a record-container format with no matrix COBOL feature ID; beta until corpus differential proof per docs/design/VB_BDW.md. ASCII-only proof; EBCDIC block payloads ride the codepage plane.",
    },
    LedgerProjection {
        id: "format.vb.block_framing",
        formats: &["vb"],
        codepages: &["all"],
        status: AssessmentStatus::Beta,
        stability: "beta",
        layers: &[AffectedLayer::Decode],
        evidence: &[
            "crates/copybook-rdw/tests/bdw_block_tests.rs::vb_single_block_roundtrip_positions",
            "crates/copybook-rdw/tests/bdw_block_tests.rs::bdw_zero_block_length_is_invalid",
            "crates/copybook-rdw/tests/bdw_block_tests.rs::bdw_truncated_block_is_underflow",
            "crates/copybook-rdw/tests/bdw_block_tests.rs::vb_rdw_beyond_block_is_rejected",
            "crates/copybook-rdw/tests/bdw_block_tests.rs::bdw_reserved_nonzero_strict_is_rejected",
        ],
        limitations: "Byte-level framing row; schema-aware behavior rides format.vb.basic. Beta until corpus proof.",
    },
    LedgerProjection {
        id: "struct.field.alphanumeric",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-core/tests/parser_comprehensive.rs::test_pic_alphanumeric",
            "crates/copybook-core/tests/layout_resolution.rs::test_size_pic_x",
            "crates/copybook-codec/tests/alpha_field_edge_cases.rs::test_alpha_full_width_5_chars",
            "crates/copybook-codec/tests/alpha_field_edge_cases.rs::test_alpha_encode_short_value_pads_right",
            "crates/copybook-codec/tests/roundtrip_fidelity.rs::rt_long_alphanumeric_with_padding",
        ],
        limitations: "No support-matrix feature covers plain alphanumeric fields, so feature_identity is none by rule. Layout and representation only: this row never validates unseen record payloads.",
    },
    LedgerProjection {
        id: "struct.field.binary_int",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-core/tests/parser_comprehensive.rs::test_pic_comp_binary",
            "crates/copybook-core/tests/layout_resolution.rs::test_size_comp_binary",
            "crates/copybook-codec/tests/comp_binary_deep.rs::test_comp_16bit_signed_zero",
            "crates/copybook-codec/tests/comp_binary_deep.rs::test_encode_decode_roundtrip_16bit_signed",
            "crates/copybook-codec/tests/comp_binary_deep.rs::test_encode_decode_roundtrip_32bit_unsigned",
        ],
        limitations: "No support-matrix feature covers COMP binary fields, so feature_identity is none by rule. Big-endian byte order per mainframe convention; layout and representation only. This row never validates unseen record payloads.",
    },
    LedgerProjection {
        id: "struct.field.display_numeric",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-core/tests/parser_comprehensive.rs::test_pic_numeric_unsigned",
            "crates/copybook-core/tests/layout_resolution.rs::test_size_pic_9",
            "crates/copybook-codec/tests/codec_roundtrip_exhaustive.rs::roundtrip_ascii_zoned_unsigned",
            "crates/copybook-codec/tests/binary_roundtrip_fidelity_tests.rs::test_ascii_zoned_roundtrip_byte_identical",
            "crates/copybook-codec/tests/codec_roundtrip_exhaustive.rs::roundtrip_ascii_zoned_unsigned",
        ],
        limitations: "No support-matrix feature covers plain display numerics, so feature_identity is none by rule. Covers unsigned display (PIC 9) layout and representation only; signed overpunch, scale handling, and unseen-payload digit validity are out of scope. This row never validates unseen record payloads.",
    },
    LedgerProjection {
        id: "struct.field.packed_decimal",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-core/tests/parser_comprehensive.rs::test_pic_comp3_packed",
            "crates/copybook-core/tests/layout_resolution.rs::test_size_comp3_packed_decimal",
            "crates/copybook-codec/tests/binary_roundtrip_fidelity_tests.rs::test_comp3_packed_decimal_roundtrip_accuracy",
            "crates/copybook-codec/tests/binary_roundtrip_fidelity_tests.rs::test_comp3_packed_decimal_roundtrip_accuracy",
            "crates/copybook-codec/tests/binary_roundtrip_fidelity_tests.rs::test_comp3_packed_decimal_roundtrip_accuracy",
        ],
        limitations: "No support-matrix feature covers COMP-3 packed fields, so feature_identity is none by rule. Covers signed, scaled, and unsigned packed layout and representation with mainframe sign-nibble conventions. This row never validates unseen record payloads.",
    },
    LedgerProjection {
        id: "struct.field.signed_zoned",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-core/tests/parser_comprehensive.rs::test_pic_numeric_signed_with_decimal",
            "crates/copybook-core/tests/layout_resolution.rs::test_size_pic_s9_overpunch",
            "crates/copybook-codec/tests/decode_comprehensive_deep.rs::decode_signed_display_negative_overpunch_ebcdic",
            "crates/copybook-codec/tests/codec_roundtrip_exhaustive.rs::roundtrip_display_numeric_ebcdic_signed_positive",
            "crates/copybook-codec/tests/codec_roundtrip_exhaustive.rs::roundtrip_display_numeric_ebcdic_signed_negative",
        ],
        limitations: "No support-matrix feature covers signed zoned fields, so feature_identity is none by rule. Overpunch sign encoding per EBCDIC/ASCII zone conventions; covers signed and V-scaled zoned layout and representation. This row never validates unseen record payloads.",
    },
    LedgerProjection {
        id: "struct.level88.codec_context",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[AffectedLayer::Decode],
        evidence: &[
            "crates/copybook-core/tests/golden_fixtures_ac2_level88_after_odo.rs::test_ac2_basic_level88_after_odo_pass",
            "crates/copybook-core/tests/golden_fixtures_ac5_redefines_level88_interactions.rs::test_ac5_basic_level88_with_redefines_pass",
        ],
        limitations: "Parent-field value relationship asserted inside the golden anchors.",
    },
    LedgerProjection {
        id: "struct.level88.invalid_reference",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Limited,
        stability: "stable",
        layers: &[],
        evidence: &[
            "crates/copybook-codec/tests/enterprise_level88_mismatch.rs::enterprise_level88_value_mismatch_decodes_without_error",
        ],
        limitations: "Value-mismatch behavior pinned; the exact stable identity for dangling condition references is pending #576.",
    },
    LedgerProjection {
        id: "struct.level88.projection_alias",
        formats: &[],
        codepages: &[],
        status: AssessmentStatus::Beta,
        stability: "beta",
        layers: &[],
        evidence: &[],
        limitations: "Beta placeholder reserving the scenario ID; selection/reference behavior must be anchored before any claim.",
    },
    LedgerProjection {
        id: "struct.level88.single_value",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
        ],
        evidence: &[
            "crates/copybook-core/tests/test_level88_comma_support.rs::test_level88_single_value_no_comma",
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::level88_is_non_storage",
            "crates/copybook-core/tests/golden_fixtures_ac2_level88_after_odo.rs::test_ac2_basic_level88_after_odo_pass",
        ],
        limitations: "Non-storage semantic asserted by the layout anchor; value evaluation rides the parent field.",
    },
    LedgerProjection {
        id: "struct.level88.value_list_range",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/test_level88_comma_support.rs::test_level88_comma_separated_string_values",
            "crates/copybook-core/tests/test_level88_comma_support.rs::test_level88_comma_with_ranges",
        ],
        limitations: "Syntax row only; evaluation semantics ride struct.level88.single_value.",
    },
    LedgerProjection {
        id: "struct.odo.counter_array_consistency",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[AffectedLayer::Parse, AffectedLayer::Encode],
        evidence: &[
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::odo_counter_drives_array_length",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_encode_counter_array_match_accepted",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_encode_counter_array_mismatch_rejected",
        ],
        limitations: "Exact mismatch rejection code to be linked during #576 reconciliation.",
    },
    LedgerProjection {
        id: "struct.odo.dialect_minimum",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/redefines_odo_deep.rs::odo_min_count_zero_tolerant",
        ],
        limitations: "Link row only; behavior detail lives in the dialect lever contract, never duplicated here.",
    },
    LedgerProjection {
        id: "struct.odo.nested",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Rejected,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/nested_odo_negative_tests.rs::test_o5_nested_odo_basic_rejection",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_nested_odo_rejection",
        ],
        limitations: "O5/O6 nesting is rejected by design; the supported O1-O4 subset needs its own row before any partial claim.",
    },
    LedgerProjection {
        id: "struct.odo.not_tail",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Rejected,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/odo_tail_validation.rs::odo_tail_fails_when_storage_sibling_follows",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_not_at_tail_rejection",
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::odo_non_tail_is_rejected",
        ],
        limitations: "Place OCCURS DEPENDING ON last in its group; the dialect lever only tunes the minimum count, never the tail rule.",
    },
    LedgerProjection {
        id: "struct.odo.over_redefines",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Rejected,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/nested_odo_negative_tests.rs::test_o6_odo_over_redefines_basic",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_driver_in_redefines_rejection",
        ],
        limitations: "Restructure so the ODO driver does not overlay a REDEFINES group; no flag or dialect escape exists.",
    },
    LedgerProjection {
        id: "struct.odo.tail_fixed",
        formats: &["fixed"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
        ],
        evidence: &[
            "crates/copybook-core/tests/odo_tail_validation.rs::odo_tail_ok_with_children_but_no_sibling_after",
            "crates/copybook-core/tests/golden_fixtures_odo.rs::golden_simple_odo_with_nesting_passes",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_valid_odo_configuration",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_encode_counter_array_match_accepted",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_encode_counter_array_mismatch_rejected",
        ],
        limitations: "Round-trip and CLI layers ride the format plane (see relationships); row-level anchors pending.",
    },
    LedgerProjection {
        id: "struct.odo.tail_rdw_variable",
        formats: &["rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
        ],
        evidence: &[
            "crates/copybook-core/tests/golden_fixtures_odo.rs::golden_child_inside_odo_passes",
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::odo_variable_length_decodes_through_rdw",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_payload_length_correctness",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_encode_counter_array_match_accepted",
            "crates/copybook-codec/tests/odo_comprehensive.rs::test_odo_encode_counter_array_mismatch_rejected",
        ],
        limitations: "Parallel proof is decode-side; layout and CLI ride linked plane rows.",
    },
    LedgerProjection {
        id: "struct.redefines.encode_ambiguity",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Limited,
        stability: "stable",
        layers: &[AffectedLayer::Encode],
        evidence: &[
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_encode_ambiguity_error",
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::redefines_encode_ambiguity_is_rejected",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_encode_ambiguity_error",
        ],
        limitations: "Ambiguous multi-view encode is refused; the exact stable identity is pending #576 reconciliation.",
    },
    LedgerProjection {
        id: "struct.redefines.group",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Layout,
            AffectedLayer::Decode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::redefines_group_overlays_original",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_decode_all_views",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_round_trip_preservation",
        ],
        limitations: "Declaration-order and named-view contract asserted by the decode anchor; see the support matrix structural note.",
    },
    LedgerProjection {
        id: "struct.redefines.nested_supported",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Limited,
        stability: "stable",
        layers: &[AffectedLayer::Decode],
        evidence: &[
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_nested_redefines_groups",
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::redefines_nested_local_cluster_advances_sibling",
        ],
        limitations: "Only the explicitly anchored combinations are supported; every other nesting shape stays rejected until anchored.",
    },
    LedgerProjection {
        id: "struct.redefines.policy_limited",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Limited,
        stability: "stable",
        layers: &[AffectedLayer::Decode],
        evidence: &[
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_raw_data_precedence",
        ],
        limitations: "Raw-data precedence is the pinned policy; unsupported interaction shapes reject in their own rows.",
    },
    LedgerProjection {
        id: "struct.redefines.scalar",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Decode,
            AffectedLayer::Encode,
            AffectedLayer::RoundTrip,
        ],
        evidence: &[
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_decode_all_views",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_encode_single_view_allowed",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_round_trip_preservation",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_encode_ambiguity_error",
        ],
        limitations: "No support-matrix feature ID exists for REDEFINES; this row owns structural status directly.",
    },
    LedgerProjection {
        id: "struct.redefines.short_equal_long",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[AffectedLayer::Decode],
        evidence: &[
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_shorter_overlay",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_equal_overlay",
            "crates/copybook-codec/tests/redefines_comprehensive.rs::test_redefines_longer_overlay",
        ],
        limitations: "Longer-overlay truncation policy asserted inside the longer-overlay anchor.",
    },
    LedgerProjection {
        id: "struct.renames.cross_occurs",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Rejected,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/renames_resolver_negative_tests.rs::renames_crosses_occurs_boundary",
            "crates/copybook-core/tests/renames_resolver_negative_tests.rs::renames_crosses_occurs_boundary",
        ],
        limitations: "Keep RENAMES ranges inside one OCCURS table; spanning names stay rejected.",
    },
    LedgerProjection {
        id: "struct.renames.invalid_range",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Rejected,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/renames_resolver_negative_tests.rs::renames_reversed_range",
            "crates/copybook-codec/tests/structural_evidence_matrix.rs::renames_reversed_range_is_rejected",
        ],
        limitations: "Order THRU after FROM within one scope; reversed or cross-scope ranges stay rejected.",
    },
    LedgerProjection {
        id: "struct.renames.over_redefines",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Limited,
        stability: "stable",
        layers: &[AffectedLayer::Parse],
        evidence: &[
            "crates/copybook-core/tests/renames_r4_r6_feature_enabled_tests.rs::test_r4_single_redefines_renames_accepted_with_feature_flag",
        ],
        limitations: "R4 accepted only under the RenamesR4R6 flag; the unflagged path rejects with CBKS609 and needs its own negative anchor.",
    },
    LedgerProjection {
        id: "struct.renames.r1_r3",
        formats: &["fixed", "rdw"],
        codepages: &["all"],
        status: AssessmentStatus::Supported,
        stability: "stable",
        layers: &[
            AffectedLayer::Parse,
            AffectedLayer::Decode,
            AffectedLayer::Encode,
        ],
        evidence: &[
            "crates/copybook-core/tests/renames_comprehensive.rs::r1_same_scope_elementary_fields",
            "crates/copybook-core/tests/renames_comprehensive.rs::r2_group_alias_preserves_structure",
            "crates/copybook-core/tests/renames_comprehensive.rs::r3_nested_group_fields",
            "crates/copybook-codec/tests/renames_codec_tests.rs::test_renames_r1_simple_decode",
            "crates/copybook-codec/tests/renames_codec_tests.rs::test_renames_r2_group_decode",
            "crates/copybook-codec/tests/renames_codec_tests.rs::test_renames_encode_skips_alias",
            "crates/copybook-codec/tests/renames_codec_tests.rs::test_renames_missing_metadata_error",
        ],
        limitations: "R1-R3 only; R4-R6 and interaction shapes live in dedicated rows.",
    },
];

/// Look up one scenario authority row by ledger ID.
#[must_use]
#[inline]
pub fn projection_for(id: &str) -> Option<&'static LedgerProjection> {
    LEDGER_PROJECTION.iter().find(|row| row.id == id)
}
