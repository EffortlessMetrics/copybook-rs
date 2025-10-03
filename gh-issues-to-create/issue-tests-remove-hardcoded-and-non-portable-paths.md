
# [Task]: Remove Hardcoded and Non-Portable Paths from Test Suite

**Issue Description**

The test suite contains several hardcoded, environment-specific paths that severely limit its portability and reliability. This makes it difficult for new contributors to run the full test suite and can cause unexpected failures in CI/CD pipelines.

There are two categories of issues:

1.  **Developer-Specific Absolute Paths**: The test file `copybook-bench/tests/ci_integration.rs` is littered with absolute paths that point to a specific developer's home directory (e.g., `/home/steven/code/Rust/copybook-rs/...`). These tests are not runnable by anyone else.

2.  **Platform-Specific Paths**: The test file `copybook-bench/tests/diagnostics.rs` reads from `/proc/cpuinfo` and `/proc/meminfo`. These paths only exist on Linux, meaning the tests will fail on other operating systems like Windows or macOS.

**Files and Locations:**

- `copybook-bench/tests/ci_integration.rs` (multiple lines)
- `copybook-bench/tests/diagnostics.rs` (lines 48, 388)

**Code Context (Examples):**

*Developer-Specific Path:*
```rust
// copybook-bench/tests/ci_integration.rs

"/home/steven/code/Rust/copybook-rs/.github/workflows/benchmark.yml",
```

*Platform-Specific Path:*
```rust
// copybook-bench/tests/diagnostics.rs

let cpu_info = std::fs::read_to_string("/proc/cpuinfo");
```

**Proposed Fix**

These hardcoded paths should be replaced with environment-agnostic solutions.

1.  **For Developer-Specific Paths**: Replace the absolute paths with relative paths based on the crate's root directory. The `CARGO_MANIFEST_DIR` environment variable, available at compile time via `env!("CARGO_MANIFEST_DIR")`, should be used to construct the correct path to project files.

    **Example Refactoring:**
    ```rust
    // copybook-bench/tests/ci_integration.rs
    use std::path::PathBuf;

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // Construct path relative to the crate root
    let workflow_path = manifest_dir.join("../../.github/workflows/benchmark.yml");

    // Use `workflow_path` instead of the hardcoded string
    ```

2.  **For Platform-Specific Paths**: The tests that rely on specific platforms should be conditionally compiled using `#[cfg]` attributes. This ensures they are only run on the operating systems where they are expected to work.

    **Example Refactoring:**
    ```rust
    // copybook-bench/tests/diagnostics.rs

    #[test]
    #[cfg(target_os = "linux")] // Only run this test on Linux
    fn test_linux_diagnostics() {
        let cpu_info = std::fs::read_to_string("/proc/cpuinfo");
        assert!(cpu_info.is_ok());

        let meminfo = std::fs::read_to_string("/proc/meminfo");
        assert!(meminfo.is_ok());
    }
    ```

Fixing these issues is crucial for making the project's test suite robust, portable, and contributor-friendly.
