// SPDX-License-Identifier: AGPL-3.0-or-later
//! Umbrella facade for the copybook-rs library surface.
//!
//! Add `copybook-rs` to `Cargo.toml` and import it as `copybook_rs` in Rust code.
//! This crate re-exports `copybook-core` and `copybook-codec` so consumers can
//! start with a single dependency and still opt into the granular crates later.
//!
//! # Example
//!
//! ```rust
//! use copybook_rs::{
//!     Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record, parse_copybook,
//! };
//!
//! let schema = parse_copybook("01 REC.\n   05 FLAG PIC X(1).\n")?;
//! let options = DecodeOptions::new()
//!     .with_codepage(Codepage::ASCII)
//!     .with_format(RecordFormat::Fixed)
//!     .with_json_number_mode(JsonNumberMode::Lossless);
//!
//! let json = decode_record(&schema, b"A", &options)?;
//! assert_eq!(json["FLAG"], "A");
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

/// Namespaced re-export of the parsing and schema layer.
pub mod core {
    pub use copybook_core::*;
}

/// Namespaced re-export of the decode/encode layer.
pub mod codec {
    pub use copybook_codec::*;
}

/// Optional namespaced re-export of the Arrow and Parquet integration crate.
#[cfg(feature = "arrow")]
pub mod arrow {
    pub use copybook_arrow::*;
}

pub use copybook_codec::*;
pub use copybook_core::*;
