// SPDX-License-Identifier: AGPL-3.0-or-later
//! Schema-aware VB framing integration.
//!
//! `copybook-rdw` owns byte-stream block framing only. Schema-derived minimum
//! payload validation belongs here, at the codec integration boundary.

use super::rdw::validate_zero_length_record;
use copybook_core::Schema;
use copybook_error::Result;

/// Validate a zero-length VB record payload against schema-derived fixed bytes.
///
/// VB shares the bare-RDW minimum-payload rule: a record shorter than the
/// schema fixed prefix is rejected before decode.
///
/// # Errors
/// Returns `CBKF221_RDW_UNDERFLOW` when the schema requires non-zero bytes.
#[must_use = "handle the VB validation result"]
#[inline]
pub fn validate_vb_zero_length_record(schema: &Schema, record_index: u64) -> Result<()> {
    validate_zero_length_record(schema, record_index)
}
