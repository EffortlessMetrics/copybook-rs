// SPDX-License-Identifier: AGPL-3.0-or-later
#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]

/// Character set conversion utilities for EBCDIC and ASCII data.
pub mod charset {
    pub use copybook_charset::*;
}

/// High-level record encode/decode workflows.
pub mod codec {
    pub use copybook_codec::*;
}

/// Deprecated compatibility alias for the charset domain types.
#[deprecated(
    since = "0.6.0",
    note = "use copybook::charset for codepage domain types"
)]
pub mod codepage {
    pub use copybook_charset::*;
}

/// Deprecated compatibility alias for feature-flag governance contracts.
///
/// Forwards through the true owner `copybook-core` (#656 Phase F) so the
/// primary facade never depends on the compat `copybook-contracts` package;
/// the re-exported names are unchanged.
#[deprecated(
    since = "0.6.0",
    note = "use copybook::core::feature_flags for flag governance"
)]
pub mod contracts {
    pub use copybook_core::feature_flags;
    pub use copybook_core::feature_flags::{
        Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
        FeatureLifecycle,
    };
}

/// COBOL copybook parsing, schema, and validation primitives.
pub mod core {
    pub use copybook_core::*;
}

/// Deprecated compatibility alias for determinism primitives.
#[deprecated(
    since = "0.6.0",
    note = "use copybook::codec::determinism for stable hash and diff checks"
)]
pub mod determinism {
    pub use copybook_codec::determinism::*;
}

/// Error types and taxonomy.
pub mod error {
    pub use copybook_error::*;
}

/// Record framing primitives: fixed-length and RDW framing.
pub mod framing {
    /// Fixed-length record framing primitives (preferred path).
    pub mod fixed {
        pub use copybook_fixed::*;
    }
    /// RDW framing primitives (preferred path).
    pub mod rdw {
        pub use copybook_rdw::*;
    }
}

/// Deprecated compatibility alias for fixed-length record framing.
#[deprecated(since = "0.6.0", note = "use copybook::framing::fixed")]
pub mod fixed {
    pub use copybook_fixed::*;
}

/// Governance interoperability contracts (single-crate surface since #656 Phase F).
pub mod governance {
    pub use copybook_governance::*;
}

/// Configuration option contracts shared across codec workflows.
#[deprecated(
    since = "0.6.0",
    note = "use copybook::codec::options for codec operation options"
)]
pub mod options {
    pub use copybook_codec::options::*;
}

/// Deprecated compatibility alias for zoned decimal overpunch primitives.
#[deprecated(since = "0.6.0", note = "use copybook::codec::numeric::overpunch")]
pub mod overpunch {
    pub use copybook_codec::numeric::overpunch::*;
}

/// Deprecated compatibility alias for RDW framing primitives.
#[deprecated(since = "0.6.0", note = "use copybook::framing::rdw")]
pub mod rdw {
    pub use copybook_rdw::*;
}

/// Record-format dispatch across fixed and RDW framing.
#[deprecated(
    since = "0.6.0",
    note = "use copybook::codec::record or copybook::codec::file::dispatch"
)]
pub mod record_io {
    pub use copybook_codec::record::*;
}

/// COBOL feature support matrix contracts.
pub mod support_matrix {
    pub use copybook_support_matrix::*;
}
