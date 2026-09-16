#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deprecated compatibility package for the 0.5 dialect API.
//!
//! New code should use [`copybook_core::dialect`] directly. The 0.6
//! compatibility aliases were removed in 0.7; this package remains
//! published under its existing name but no longer forwards items.

#![allow(clippy::missing_inline_in_public_items)]
