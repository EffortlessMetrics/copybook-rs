// SPDX-License-Identifier: AGPL-3.0-or-later
//! Audit error-code coverage: codes whose only trigger lives in `copybook-audit`.
//!
//! Moved out of `copybook-core` per #656 Phase E (audit leaves the stable core).

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_audit::performance::BaselineManager;
use copybook_error::ErrorCode;

// =============================================================================
// CBKA001_BASELINE_ERROR: Performance baseline I/O error
// =============================================================================

/// Test CBKA001: Loading baseline from non-existent file
#[test]
fn test_cbka001_baseline_load_missing_file() {
    let manager = BaselineManager::new("/nonexistent/path/baseline.json");
    let result = manager.load_baseline();

    assert!(result.is_err(), "Expected CBKA001 for missing file");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKA001_BASELINE_ERROR,
        "Expected CBKA001_BASELINE_ERROR, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("Failed to read"),
        "Error should mention read failure: {}",
        err.message
    );
}
