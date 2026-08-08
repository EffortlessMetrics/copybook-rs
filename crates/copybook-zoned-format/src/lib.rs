#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compatibility facade for the codec-owned zoned encoding format contract.
//!
//! New code should use [`copybook_codec::numeric::zoned`] directly. This
//! package remains available for consumers migrating from the pre-convergence
//! microcrate.

pub use copybook_codec::numeric::zoned::{ParseZonedEncodingFormatError, ZonedEncodingFormat};
