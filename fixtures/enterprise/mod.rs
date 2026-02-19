// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise Test Fixtures for copybook-rs
//!
//! Provides comprehensive test data for enterprise mainframe data processing scenarios
//! Includes audit, compliance, and Issue #52 machine-readable benchmark reporting fixtures

pub mod audit;
pub mod issue_52;

pub use audit::*;
pub use issue_52::*;