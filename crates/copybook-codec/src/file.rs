// SPDX-License-Identifier: AGPL-3.0-or-later
//! Schema-aware file processing integration.

/// Operation-level dispatch across fixed, RDW, and VB framing.
pub mod dispatch;
pub mod fixed;
/// Schema-aware RDW framing integration.
pub mod rdw;
/// Schema-aware VB framing integration.
pub mod vb;
