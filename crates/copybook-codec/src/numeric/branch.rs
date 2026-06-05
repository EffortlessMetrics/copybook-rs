// SPDX-License-Identifier: AGPL-3.0-or-later
//! Branch-prediction helpers for numeric hot paths.
//!
//! This submodule isolates low-level optimization hints from decimal, packed,
//! and binary codec business logic.

/// Branch prediction hint for likely-true conditions
///
/// Provides a manual branch prediction hint to the compiler that the condition
/// is likely to be true. This optimization helps keep hot paths efficient by
/// marking the false case as cold.
///
/// # Arguments
/// * `b` - Boolean condition to evaluate
///
/// # Returns
/// The input boolean value unchanged
///
/// # Performance
/// This function is critical for COBOL data decoding hot paths where valid
/// data is the common case and errors are exceptional.
///
/// # Examples
/// ```text
/// use copybook_codec::numeric::likely;
///
/// let valid_data = true;
/// if likely(valid_data) {
///     // Hot path - optimized for this case
/// }
/// ```
#[inline]
pub(crate) fn likely(b: bool) -> bool {
    if b {
        true
    } else {
        cold_branch_hint();
        false
    }
}

/// Branch prediction hint for unlikely-true conditions
///
/// Provides a manual branch prediction hint to the compiler that the condition
/// is unlikely to be true. This optimization keeps error paths cold and hot
/// paths optimized.
///
/// # Arguments
/// * `b` - Boolean condition to evaluate
///
/// # Returns
/// The input boolean value unchanged
///
/// # Performance
/// Critical for error handling in COBOL numeric decoding where validation
/// failures are exceptional cases.
///
/// # Examples
/// ```text
/// use copybook_codec::numeric::unlikely;
///
/// let error_condition = false;
/// if unlikely(error_condition) {
///     // Cold path - marked as unlikely
/// }
/// ```
#[inline]
pub(crate) fn unlikely(b: bool) -> bool {
    if b {
        cold_branch_hint();
        true
    } else {
        false
    }
}

/// Manual branch prediction hint for cold paths
///
/// This function serves as a branch prediction hint that the calling path is cold/unlikely.
/// The `#[cold]` attribute tells the compiler this is an unlikely execution path, and
/// `#[inline(never)]` ensures the cold path doesn't bloat the hot path.
#[cold]
#[inline(never)]
fn cold_branch_hint() {
    // The #[cold] attribute tells the compiler this is an unlikely execution path.
}
