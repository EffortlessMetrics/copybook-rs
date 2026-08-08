// SPDX-License-Identifier: AGPL-3.0-or-later
//! Schema-aware file processing integration.

/// Operation-level dispatch across fixed and RDW framing.
pub mod dispatch;
pub mod fixed;
/// Schema-aware RDW framing integration.
pub mod rdw;
