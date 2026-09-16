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

/// COBOL copybook parsing, schema, and validation primitives.
pub mod core {
    pub use copybook_core::*;
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

/// Governance interoperability contracts (single-crate surface since #656 Phase F).
pub mod governance {
    pub use copybook_governance::*;
}

/// COBOL feature support matrix contracts.
pub mod support_matrix {
    pub use copybook_support_matrix::*;
}
