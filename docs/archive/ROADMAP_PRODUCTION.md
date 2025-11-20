
# Roadmap: MVP to Production-Ready

This document outlines the tasks required to move the `copybook-rs` project from an MVP state to a fully production-ready system. An MVP is considered stable and functionally correct, but it may lack complete features, comprehensive testing, and long-term maintainability.

This phase focuses on hardening the software by completing stubbed-out features, improving the health of the test suite, and reducing technical debt.

---

## Phase 2: Hardening for Production

**Status:** ✅ Done — landing pedantic RDW iterator hardening and telemetry alignment closes the phase.

### Theme: Feature Completion, Robustness, and Maintainability

The goal of this phase is to prepare the software for a full production launch with confidence in its performance, security, and long-term health.

### Key Areas of Focus

#### 1. Complete Core Features

These tasks involve finishing major features that are currently stubbed out.

-   **Implement Performance Auditor**
    -   **Description**: The performance auditing system is currently a placeholder. This task involves implementing the logic for baseline management and regression detection to enable automated performance testing.
    -   **Reference**: `gh-issues-to-create/issue-core-implement-performance-auditor-fields.md`

-   **Implement Compliance Validator Configurations**
    -   **Description**: The compliance validators (`SOX`, `HIPAA`, etc.) do not currently use their configuration flags. This task involves implementing the logic to make these validators fully configurable.
    -   **Reference**: `gh-issues-to-create/issue-core-implement-compliance-validator-configs.md`

#### 2. Improve Test Suite Health and CI/CD

These tasks focus on making the test suite more reliable, portable, and comprehensive.

-   **Manage and Document Ignored Tests**
    -   **Description**: The project has several long-running tests that are ignored by default. This task involves creating a documented process and command for running these tests to ensure they are not neglected.
    -   **Reference**: `gh-issues-to-create/issue-tests-manage-ignored-tests.md`

-   **Remove Hardcoded and Non-Portable Paths from Tests**
    -   **Description**: The test suite contains developer-specific and platform-specific absolute paths, making tests fail for other contributors and in CI. These must be replaced with environment-agnostic solutions.
    -   **Reference**: `gh-issues-to-create/issue-tests-remove-hardcoded-and-non-portable-paths.md`

#### 3. Reduce Technical Debt and Improve Code Hygiene

These tasks focus on cleaning up the codebase to improve long-term maintainability.

-   **Refine `dead_code` Allowances**
    -   **Description**: Replace broad, file-level `allow(dead_code)` attributes with specific, justified allowances on public API items, and remove any truly dead code.
    -   **Reference**: `gh-issues-to-create/issue-gen-remove-broad-dead-code-allowance.md`

-   **General Code Cleanup**
    -   **Description**: A collection of smaller cleanup tasks, including removing unused helper functions, fixing incomplete test assertions, and removing other unnecessary `allow` attributes.
    -   **References**:
        -   `gh-issues-to-create/issue-core-remove-unused-performance-helpers.md`
        -   `gh-issues-to-create/issue-codec-remove-unnecessary-dead-code-allowance.md`
        -   `gh-issues-to-create/issue-codec-incomplete-test-assertion.md`

#### 4. Enhance Features

This involves improving existing functionality based on identified gaps.

-   **Add Comma Support to Parser `VALUE` Clauses**
    -   **Description**: Enhance the copybook parser to support comma-separated values in level-88 `VALUE` clauses, making it more robust.
    -   **Reference**: `gh-issues-to-create/issue-parser-value-clause-comma-support.md`

### Production-Ready Exit Criteria

- All major features are fully implemented and configurable.
- The full test suite, including ignored tests, runs successfully in a clean environment.
- The codebase is free of significant technical debt and unnecessary compiler warnings.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
