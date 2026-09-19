// SPDX-License-Identifier: AGPL-3.0-or-later
//! Codec-owned physical execution policy.
//!
//! [`ExecutionPolicy`] carries the reviewed framing/resource policy (reserved-byte
//! handling today, the record-length bound for [#1129]) alongside the stable
//! [`DecodeOptions`](super::DecodeOptions) surface without changing it: adding a
//! field to `DecodeOptions` would break downstream struct literals, so profile
//! execution travels in this sidecar instead. Legacy entrypoints keep their
//! signatures and run under [`ExecutionPolicy::direct`]; additive `*_with_policy`
//! entrypoints and [`RecordIterator::with_policy`](crate::iterator::RecordIterator::with_policy)
//! accept a reviewed policy built by [`ExecutionPolicy::reviewed`].
//!
//! Convergence: this seam is intentionally narrow so the future `InputPlan`
//! ([#1008]) can absorb it — `reserved_strict` maps to the plan's framing
//! strictness and `maximum_record_length` maps to the plan's record bound.
//! When `InputPlan` lands, `ExecutionPolicy` retires into a constructor for it
//! rather than becoming a second permanent plan.
//!
//! [#1129]: https://github.com/EffortlessMetrics/copybook-rs/issues/1129
//! [#1008]: https://github.com/EffortlessMetrics/copybook-rs/issues/1008

use super::RecordFormat;
use copybook_error::{Error, ErrorCode, ErrorContext};
use core::fmt;

/// Upper bound profile documents may declare for
/// [`ExecutionPolicy::reviewed`]; mirrors the profile schema limit.
pub const MAX_POLICY_RECORD_LENGTH: u64 = 16 * 1024 * 1024;

/// Physical execution policy: reviewed framing/resource interpretation.
///
/// Constructed only through [`ExecutionPolicy::direct`] (legacy behavior) or
/// [`ExecutionPolicy::reviewed`] (validated profile values), never by struct
/// literal, so new policy dimensions stay additive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExecutionPolicy {
    /// Enforce zero framing reserved bytes independently of broad strict mode.
    reserved_strict: bool,
    /// Maximum logical record payload bytes, when a reviewed profile caps it.
    /// `None` selects architectural behavior; enforcement lands in [#1129].
    ///
    /// [#1129]: https://github.com/EffortlessMetrics/copybook-rs/issues/1129
    maximum_record_length: Option<u64>,
}

impl ExecutionPolicy {
    /// Legacy behavior: no profile cap, reserved strictness follows broad
    /// strict mode exactly as `DecodeOptions` did before the seam.
    #[must_use]
    #[inline]
    pub const fn direct(strict_mode: bool) -> Self {
        Self {
            reserved_strict: strict_mode,
            maximum_record_length: None,
        }
    }

    /// Validated reviewed policy from profile parts.
    ///
    /// # Errors
    ///
    /// Returns [`PolicyError`] when the bound is zero or exceeds the profile
    /// schema maximum; callers must fail before I/O rather than run uncapped.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub const fn reviewed(
        reserved_strict: bool,
        maximum_record_length: u64,
    ) -> Result<Self, PolicyError> {
        if maximum_record_length == 0 {
            return Err(PolicyError::EmptyRecordBound);
        }
        if maximum_record_length > MAX_POLICY_RECORD_LENGTH {
            return Err(PolicyError::RecordBoundExceedsMaximum {
                value: maximum_record_length,
            });
        }
        Ok(Self {
            reserved_strict,
            maximum_record_length: Some(maximum_record_length),
        })
    }

    /// Whether this policy enforces zero framing reserved bytes on its own.
    #[must_use]
    #[inline]
    pub const fn reserved_strict(self) -> bool {
        self.reserved_strict
    }

    /// The reviewed record bound, when one was supplied.
    #[must_use]
    #[inline]
    pub const fn maximum_record_length(self) -> Option<u64> {
        self.maximum_record_length
    }

    /// Framing-level strictness for RDW/BDW reader construction.
    ///
    /// Reserved-byte enforcement follows either broad strict mode or the
    /// reviewed reserved policy; record-error fatality still follows only
    /// broad strict mode.
    #[must_use]
    #[inline]
    pub const fn framing_strict(self, strict_mode: bool) -> bool {
        strict_mode || self.reserved_strict
    }

    /// Reject a fixed layout whose LRECL exceeds the reviewed bound.
    ///
    /// Runs before any input is consumed, so a profile/layout mismatch
    /// leaves output absent. A layout that fits (or no reviewed bound)
    /// passes silently.
    ///
    /// # Errors
    ///
    /// Returns `CBKF226_RECORD_BOUND_EXCEEDED` when the layout requires
    /// more payload bytes per record than the reviewed bound allows.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn check_fixed_lrecl(self, lrecl: u32) -> Result<(), Error> {
        let Some(cap) = self.maximum_record_length else {
            return Ok(());
        };
        if u64::from(lrecl) <= cap {
            return Ok(());
        }
        Err(Error::new(
            ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED,
            format!(
                "fixed layout requires {lrecl} bytes per record, exceeding the reviewed bound of {cap}"
            ),
        )
        .with_context(ErrorContext {
            record_index: None,
            field_path: None,
            byte_offset: Some(0),
            line_number: None,
            details: Some(format!("lrecl {lrecl}, bound {cap}")),
        }))
    }

    /// Reject an encoded logical payload longer than the reviewed bound.
    ///
    /// Runs after field encoding (or raw-payload validation) but before any
    /// record header/payload pair is written, so an over-cap record never
    /// crosses the output boundary. A payload that fits (or no reviewed
    /// bound) passes silently.
    ///
    /// # Errors
    ///
    /// Returns `CBKF226_RECORD_BOUND_EXCEEDED` when the produced logical
    /// payload exceeds the reviewed bound.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn check_encoded_payload(
        self,
        payload_len: usize,
        format: RecordFormat,
        record_index: Option<u64>,
    ) -> Result<(), Error> {
        let Some(cap) = self.maximum_record_length else {
            return Ok(());
        };
        if u64::try_from(payload_len).is_ok_and(|declared| declared <= cap) {
            return Ok(());
        }
        Err(Error::new(
            ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED,
            format!(
                "encode record{} produces {payload_len} payload bytes, exceeding the reviewed bound of {cap} ({format:?} payload)",
                record_index.map_or(String::new(), |index| format!(" {index}")),
                cap = cap,
            ),
        )
        .with_context(ErrorContext {
            record_index,
            field_path: None,
            byte_offset: None,
            line_number: None,
            details: Some(format!("produced {payload_len}, bound {cap}")),
        }))
    }
}

/// Failure to build a reviewed [`ExecutionPolicy`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PolicyError {
    /// A zero record bound caps nothing; refuse before I/O.
    EmptyRecordBound,
    /// The bound exceeds the profile schema maximum (`16 MiB`).
    RecordBoundExceedsMaximum {
        /// Value found in the profile.
        value: u64,
    },
}

impl fmt::Display for PolicyError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyRecordBound => {
                write!(formatter, "record bound must be at least 1 byte")
            }
            Self::RecordBoundExceedsMaximum { value } => write!(
                formatter,
                "record bound {value} exceeds the profile maximum of {MAX_POLICY_RECORD_LENGTH}"
            ),
        }
    }
}

impl std::error::Error for PolicyError {}
