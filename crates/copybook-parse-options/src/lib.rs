#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Parse option contracts shared by copybook parser components.

use copybook_dialect::Dialect;
use serde::{Deserialize, Serialize};

/// Options for controlling COBOL copybook parsing behavior.
///
/// Configures how the parser handles various COBOL dialect features,
/// comment styles, and validation strictness.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
pub struct ParseOptions {
    /// Whether to emit FILLER fields in the parsed schema output.
    pub emit_filler: bool,
    /// Codepage identifier used for fingerprint calculation (e.g., `"cp037"`).
    pub codepage: String,
    /// Whether to allow COBOL-2002 inline comments (`*>`).
    pub allow_inline_comments: bool,
    /// Whether to run in strict mode with less error tolerance.
    pub strict: bool,
    /// Whether to enforce strict comment parsing rules.
    pub strict_comments: bool,
    /// Dialect for ODO `min_count` interpretation.
    pub dialect: Dialect,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            emit_filler: false,
            codepage: "cp037".to_string(),
            allow_inline_comments: true,
            strict: false,
            strict_comments: false,
            dialect: Dialect::Normative,
        }
    }
}
