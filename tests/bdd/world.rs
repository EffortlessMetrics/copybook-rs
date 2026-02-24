// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::determinism::DeterminismResult;
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat};
use copybook_core::{ParseOptions, parse_copybook, parse_copybook_with_options};
use serde_json::Value;

use crate::helpers::{default_ascii_decode_options, default_ascii_encode_options};

/// BDD World struct to maintain test state across steps
#[derive(Debug, Default, cucumber::World)]
pub struct CopybookWorld {
    /// The copybook text being parsed
    pub(crate) copybook_text: Option<String>,
    /// The parsed schema
    pub(crate) schema: Option<copybook_core::Schema>,
    /// The projected schema (after field projection)
    pub(crate) projected_schema: Option<copybook_core::Schema>,
    /// The binary data for encoding/decoding
    pub(crate) binary_data: Option<Vec<u8>>,
    /// The JSON data for encoding/decoding
    pub(crate) json_data: Option<String>,
    /// The decoded JSON output
    pub(crate) decoded_output: Option<String>,
    /// The encoded binary output
    pub(crate) encoded_output: Option<Vec<u8>>,
    /// Any error that occurred
    pub(crate) error: Option<copybook_core::Error>,
    /// The current parse options
    pub(crate) parse_options: Option<ParseOptions>,
    /// The current decode options
    pub(crate) decode_options: Option<DecodeOptions>,
    /// The current encode options
    pub(crate) encode_options: Option<copybook_codec::EncodeOptions>,
    /// Record count for multi-record tests
    pub(crate) record_count: Option<usize>,
    /// The current dialect for ODO parsing
    pub(crate) dialect: Option<copybook_core::dialect::Dialect>,
    /// Field selection for projection
    pub(crate) field_selection: Option<Vec<String>>,
    /// The determinism check result
    pub(crate) determinism_result: Option<DeterminismResult>,
    /// Verify report output
    pub(crate) verify_report: Option<String>,
    /// Verify error count
    pub(crate) verify_error_count: Option<usize>,
    /// Baseline output for cross-thread comparison
    pub(crate) baseline_output: Option<String>,
    /// Exit code result
    pub(crate) exit_code_result: Option<i32>,
    /// Last zoned encoding detected from a byte probe
    pub(crate) detected_zoned_encoding: Option<copybook_codec::ZonedEncodingFormat>,
    /// Safe-op scratch state for BDD coverage of `copybook-safe-ops`.
    pub(crate) safe_ops_input: Option<String>,
    pub(crate) safe_ops_usize: Option<usize>,
    pub(crate) safe_ops_isize: Option<isize>,
    pub(crate) safe_ops_u16: Option<u16>,
    pub(crate) safe_ops_char: Option<char>,
    pub(crate) safe_ops_array_bound: Option<usize>,
    pub(crate) safe_ops_error_code: Option<copybook_core::ErrorCode>,
    /// RDW ASCII heuristic result for BDD coverage.
    pub(crate) rdw_predicate_result: Option<bool>,
    /// Audit-related fields (for enterprise audit testing)
    #[cfg(feature = "audit")]
    pub(crate) audit_context: Option<copybook_core::audit::AuditContext>,
    #[cfg(feature = "audit")]
    pub(crate) audit_events: Vec<copybook_core::audit::AuditEvent>,
    #[cfg(feature = "audit")]
    pub(crate) audit_output: Option<String>,
    #[cfg(feature = "audit")]
    pub(crate) compliance_profile: Option<copybook_core::audit::ComplianceProfile>,
    #[cfg(feature = "audit")]
    pub(crate) security_classification:
        Option<copybook_core::audit::context::SecurityClassification>,
    #[cfg(feature = "audit")]
    pub(crate) child_audit_context: Option<copybook_core::audit::AuditContext>,
    #[cfg(feature = "audit")]
    pub(crate) regression_metrics: bool,
}

impl CopybookWorld {
    pub(crate) fn ensure_schema_parsed(&mut self) {
        if self.schema.is_some() || self.error.is_some() {
            return;
        }

        let copybook_text = self.copybook_text.as_ref().expect("Copybook text not set");

        match &self.parse_options {
            Some(options) => match parse_copybook_with_options(copybook_text, options) {
                Ok(schema) => {
                    self.schema = Some(schema);
                }
                Err(e) => {
                    self.error = Some(e);
                }
            },
            None => match parse_copybook(copybook_text) {
                Ok(schema) => {
                    self.schema = Some(schema);
                }
                Err(e) => {
                    self.error = Some(e);
                }
            },
        }
    }

    pub(crate) fn ensure_decode_options(&mut self) {
        if self.decode_options.is_none() {
            self.decode_options = Some(default_ascii_decode_options());
        }
    }

    pub(crate) fn ensure_encode_options(&mut self) {
        if self.encode_options.is_none() {
            self.encode_options = Some(default_ascii_encode_options());
        }
    }

    pub(crate) fn ensure_schema_and_return(&mut self) -> bool {
        self.ensure_schema_parsed();
        self.schema.is_some()
    }

    pub(crate) fn schema(&self) -> &copybook_core::Schema {
        self.schema.as_ref().expect("Schema not parsed")
    }

    pub(crate) fn find_field_by_name(&self, name: &str) -> Option<&copybook_core::Field> {
        self.schema().find_field(name)
    }

    pub(crate) fn find_field_by_name_ci(&self, name: &str) -> Option<&copybook_core::Field> {
        self.schema().find_field(name)
    }

    pub(crate) fn all_leaf_fields(&self) -> Vec<&copybook_core::Field> {
        self.schema()
            .all_fields()
            .into_iter()
            .filter(|f| {
                f.children.is_empty()
                    && !matches!(
                        f.kind,
                        copybook_core::FieldKind::Condition { .. }
                            | copybook_core::FieldKind::Renames { .. }
                    )
            })
            .collect()
    }

    pub(crate) fn first_leaf_field(&self) -> Option<&copybook_core::Field> {
        self.all_leaf_fields().into_iter().next()
    }

    pub(crate) fn first_numeric_leaf_field(&self) -> Option<&copybook_core::Field> {
        self.all_leaf_fields().into_iter().find(|field| {
            matches!(
                field.kind,
                copybook_core::FieldKind::ZonedDecimal { .. }
                    | copybook_core::FieldKind::BinaryInt { .. }
                    | copybook_core::FieldKind::PackedDecimal { .. }
                    | copybook_core::FieldKind::EditedNumeric { .. }
                    | copybook_core::FieldKind::FloatSingle
                    | copybook_core::FieldKind::FloatDouble
            )
        })
    }

    pub(crate) fn renames_field(&self) -> Option<&copybook_core::Field> {
        self.schema().fields.iter().find(|field| field.level == 66)
    }

    pub(crate) fn first_decoded_record(&self) -> Value {
        let output = self
            .decoded_output
            .as_ref()
            .expect("Decoded output not set");
        for line in output.lines() {
            if !line.trim().is_empty() {
                return serde_json::from_str(line).expect("Invalid JSON in decoded output");
            }
        }
        panic!("No records in decoded output");
    }

    pub(crate) fn field_in_projection(&self, field_name: &str) -> bool {
        let schema = self
            .projected_schema
            .as_ref()
            .expect("Projected schema not set");
        Self::find_field_recursive(schema, field_name)
    }

    fn find_field_recursive(schema: &copybook_core::Schema, field_name: &str) -> bool {
        for field in &schema.fields {
            if field.name.eq_ignore_ascii_case(field_name) {
                return true;
            }
            if !field.children.is_empty() {
                let child_schema = copybook_core::Schema {
                    fields: field.children.clone(),
                    ..Default::default()
                };
                if Self::find_field_recursive(&child_schema, field_name) {
                    return true;
                }
            }
        }
        false
    }
}
