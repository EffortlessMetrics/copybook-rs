// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compile-contract coverage for the deprecated 0.5 package path.
#![allow(deprecated)]

use copybook_dialect::{Dialect, effective_min_count};
use std::str::FromStr;

#[test]
fn compatibility_aliases_preserve_dialect_values() {
    assert_eq!(Dialect::default(), Dialect::Normative);
    assert_eq!(Dialect::from_str("1").unwrap(), Dialect::OneTolerant);
}

#[test]
fn compatibility_helper_forwards_to_core_owner() {
    assert_eq!(
        effective_min_count(Dialect::OneTolerant, 0),
        copybook_core::dialect::effective_min_count(
            copybook_core::dialect::Dialect::OneTolerant,
            0,
        )
    );
}
