// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe utility functions and extension traits
//!
//! This module is deprecated in favor of the copybook-utils crate.
//! All functionality is now re-exported from copybook-utils for backward compatibility.

// Re-export everything from copybook_utils for backward compatibility
pub use copybook_utils::{OptionExt, SliceExt, VecExt, safe_ops};
