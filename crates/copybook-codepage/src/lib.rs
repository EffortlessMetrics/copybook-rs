#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compatibility forwarding package for the charset domain types.
//!
//! New code should depend on `copybook-charset`. This package remains as an
//! implementation-free migration shim for the published 0.5 API.

#[deprecated(
    since = "0.6.0",
    note = "use copybook_charset for codepage domain types"
)]
pub use copybook_charset::{
    Codepage, ParseCodepageError, ParseUnmappablePolicyError, UnmappablePolicy,
    get_zoned_sign_table, space_byte,
};
