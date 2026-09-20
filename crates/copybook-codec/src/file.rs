// SPDX-License-Identifier: AGPL-3.0-or-later
//! Schema-aware file processing integration.

/// Operation-level dispatch across fixed, RDW, VB, and text framing.
pub mod dispatch;
pub mod fixed;
/// Schema-aware RDW framing integration.
pub mod rdw;
/// Line-delimited text framing for fixed-width records.
pub mod text;
/// Schema-aware VB framing integration.
pub mod vb;
