#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deprecated compatibility package for the 0.5 dialect API.
//!
//! New code should use [`copybook_core::dialect`] directly. This package is
//! retained through the 0.6 compatibility window because the package name was
//! already published.

#![allow(clippy::missing_inline_in_public_items)]

/// Deprecated alias for [`copybook_core::dialect::Dialect`].
#[deprecated(since = "0.6.0", note = "use copybook_core::dialect::Dialect instead")]
pub type Dialect = copybook_core::dialect::Dialect;

/// Deprecated forwarding helper for [`copybook_core::dialect::effective_min_count`].
#[deprecated(
    since = "0.6.0",
    note = "use copybook_core::dialect::effective_min_count instead"
)]
#[allow(deprecated)]
#[inline]
#[must_use]
pub fn effective_min_count(dialect: Dialect, declared_min_count: u32) -> u32 {
    copybook_core::dialect::effective_min_count(dialect, declared_min_count)
}
