#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compatibility forwarding package for the charset domain types.
//!
//! New code should depend on `copybook-charset`. This package remains as an
//! implementation-free migration shim for the published 0.5 API.
#![allow(clippy::missing_inline_in_public_items)]

/// Deprecated alias for [`copybook_charset::Codepage`].
#[deprecated(
    since = "0.6.0",
    note = "use copybook_charset for codepage domain types"
)]
pub type Codepage = copybook_charset::Codepage;

/// Deprecated alias for [`copybook_charset::ParseCodepageError`].
#[deprecated(since = "0.6.0", note = "use copybook_charset::ParseCodepageError")]
pub type ParseCodepageError = copybook_charset::ParseCodepageError;

/// Deprecated alias for [`copybook_charset::ParseUnmappablePolicyError`].
#[deprecated(
    since = "0.6.0",
    note = "use copybook_charset::ParseUnmappablePolicyError"
)]
pub type ParseUnmappablePolicyError = copybook_charset::ParseUnmappablePolicyError;

/// Deprecated alias for [`copybook_charset::UnmappablePolicy`].
#[deprecated(since = "0.6.0", note = "use copybook_charset::UnmappablePolicy")]
pub type UnmappablePolicy = copybook_charset::UnmappablePolicy;

/// Deprecated forwarding wrapper for [`copybook_charset::space_byte`].
#[deprecated(since = "0.6.0", note = "use copybook_charset::space_byte")]
#[must_use]
pub const fn space_byte(codepage: copybook_charset::Codepage) -> u8 {
    copybook_charset::space_byte(codepage)
}

/// Deprecated forwarding wrapper for [`copybook_charset::get_zoned_sign_table`].
#[deprecated(since = "0.6.0", note = "use copybook_charset::get_zoned_sign_table")]
#[must_use]
pub fn get_zoned_sign_table(codepage: copybook_charset::Codepage) -> &'static [(bool, bool); 16] {
    copybook_charset::get_zoned_sign_table(codepage)
}
