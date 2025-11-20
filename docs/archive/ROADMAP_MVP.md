
# Roadmap: Current State to MVP

This document outlines the critical tasks required to move the `copybook-rs` project from its current state to a Minimum Viable Product (MVP). The focus of this phase is to address all "launch blocker" issues, ensuring the core library is stable, reliable, and produces correct results.

An MVP is defined as a version of the software that can be used for integration and end-to-end testing without risk of data corruption or critical crashes in its primary functionality.

---

## Phase 1: Achieving MVP

### Theme: Stability and Correctness

The goal of this phase is to fix all known critical bugs that would prevent a safe and successful initial deployment.

### Critical Tasks for MVP

1.  **Fix Data Corruption Bug in Zoned Decimal Encoding**
    -   **Description**: The current implementation of ASCII overpunch sign handling is incorrect, leading to silent data corruption when decoding zoned decimal fields. This is the highest priority issue.
    -   **Reference**: `gh-issues-to-create/issue-codec-ascii-overpunch-d-mapping.md`

2.  **Eliminate Panics from Production Library Code**
    -   **Description**: The core libraries (`copybook-core` and `copybook-codec`) contain numerous `.unwrap()` and `.expect()` calls that can cause the application to crash. A systematic audit must be performed to replace all potential panics in library code with robust error handling.
    -   **Reference**: `gh-issues-to-create/issue-epic-audit-panics-in-production-code.md`

### MVP Exit Criteria

- All ASCII overpunch sign mappings are verified and corrected.
- The core libraries are free of panics in all primary code paths.
- The test suite passes, demonstrating the fixes are effective.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
