
# [Task]: Document and Manage Ignored Tests

**Issue Description**

There are at least 8 tests across the repository that are marked with the `#[ignore]` attribute. The comments indicate these are ignored primarily because they are long-running or require special external dependencies (e.g., `cargo-audit`).

While ignoring slow tests in the default test run is a standard practice, it creates a risk that these tests are forgotten and no longer run regularly. This can lead to regressions in performance, documentation, or security that go undetected.

**List of Ignored Tests:**

- `tests/test_documentation_ac6.rs`: `comprehesive_documentation_generation_and_validation`
- `tests/production_readiness_ac5.rs`: `comprehensive_production_readiness_validation`
- `tests/performance_validation_ac2.rs`: `enterprise_scale_performance_validation`
- `tests/enterprise_stress_ac4.rs`: `enterprise_scale_stress_test`
- `tests/enterprise_fixtures_ac1.rs`: `comprehensive_enterprise_fixture_generation`
- `tests/comprehensive_fixtures_ac8.rs`: `comprehensive_fixture_system_validation`
- `tests/binary_fidelity_ac3.rs`: `enterprise_scale_binary_fidelity_validation`
- `fixtures/panic_elimination/performance_stress_fixtures.rs`: `test_stress_fixture_generation`

**Proposed Actions**

To ensure these important tests are still utilized effectively, their management should be improved.

1.  **Create a Documented Test Command**:
    A command should be added to the project's `justfile` to make running all ignored tests easy and explicit. This provides a single, memorable way to execute the full test suite.

    **Example `justfile` entry:**
    ```just
    # Run all tests, including long-running and ignored tests.
    test-all:
        cargo test -- --ignored
    ```

2.  **Update `CONTRIBUTING.md` or a Testing Guide**:
    The project's documentation should be updated to:
    -   Explain the purpose of the ignored tests.
    -   Document the new `just test-all` command.
    -   Provide clear instructions on how to set up any required external dependencies (like `cargo-audit` and `cargo-geiger` for the security tests).

3.  **Consider a Separate CI Job**:
    If not already in place, a separate, non-blocking CI job could be created that runs nightly or weekly. This job would execute the `just test-all` command to ensure that long-running tests are not allowed to fail for long periods.

By implementing these changes, the project can benefit from its comprehensive test suite without slowing down the default development and CI cycle, while also ensuring that critical performance, stress, and security tests are not neglected.
