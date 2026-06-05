// SPDX-License-Identifier: AGPL-3.0-or-later
//! Thread-local warning accounting for decode runs.

use std::cell::RefCell;

thread_local! {
    static WARNING_COUNTER: RefCell<u64> = const { RefCell::new(0) };
}

/// Increment the thread-local warning counter.
///
/// Called internally when non-fatal issues (e.g., BWZ blanks, ODO clamping)
/// are encountered during decode. The count is aggregated into [`crate::lib_api::RunSummary::warnings`].
pub fn increment_warning_counter() {
    WARNING_COUNTER.with(|counter| {
        *counter.borrow_mut() += 1;
    });
}

#[inline]
pub(super) fn reset_warning_counter() {
    WARNING_COUNTER.with(|counter| {
        *counter.borrow_mut() = 0;
    });
}

#[inline]
pub(super) fn warning_count() -> u64 {
    WARNING_COUNTER.with(|counter| *counter.borrow())
}
