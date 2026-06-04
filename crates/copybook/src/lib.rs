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

/// Codepage and unmappable-character policy types.
pub mod codepage {
    pub use copybook_codepage::*;
}

/// Shared feature-flag governance contracts.
pub mod contracts {
    pub use copybook_contracts::*;
}

/// COBOL copybook parsing, schema, and validation primitives.
pub mod core {
    pub use copybook_core::*;
}

/// Determinism primitives for stable hash and diff comparison.
pub mod determinism {
    pub use copybook_determinism::*;
}

/// Error types and taxonomy.
pub mod error {
    pub use copybook_error::*;
}

/// Structured error reporting policies and summaries.
pub mod error_reporter {
    pub use copybook_error_reporter::*;
}

/// Fixed-length record framing primitives.
pub mod fixed {
    pub use copybook_fixed::*;
}

/// Governance interoperability contracts.
pub mod governance_contracts {
    pub use copybook_governance_contracts::*;
}

/// Configuration option contracts shared across codec workflows.
pub mod options {
    pub use copybook_options::*;
}

/// Overflow-safe integer narrowing and bounds arithmetic.
pub mod overflow {
    pub use copybook_overflow::*;
}

/// Zoned decimal overpunch encode/decode primitives.
pub mod overpunch {
    pub use copybook_overpunch::*;
}

/// RDW framing primitives.
pub mod rdw {
    pub use copybook_rdw::*;
}

/// Record-format dispatch across fixed and RDW framing.
pub mod record_io {
    pub use copybook_record_io::*;
}

/// COBOL feature support matrix contracts.
pub mod support_matrix {
    pub use copybook_support_matrix::*;
}

/// Panic-safe utility functions and extension traits.
pub mod utils {
    pub use copybook_utils::*;
}
