# [Task]: Remove Unused Performance Helper Functions in `copybook-core/src/utils.rs`

**Issue Description**

The file `copybook-core/src/utils.rs` contains two helper functions, `likely` and `cold_path`, which are intended for performance optimization through branch prediction hinting. However, a search of the `copybook-core` crate confirms that these functions are **never called** outside of their own definitions.

They are currently marked with `#[allow(dead_code)]`, which suppresses compiler warnings about them being unused. This dead code adds to the cognitive load of developers and can cause confusion, especially since a *different*, actively used `likely` function exists in the `copybook-codec` crate.

**File and Location:**

- `copybook-core/src/utils.rs:375`
- `copybook-core/src/utils.rs:389`

**Code Context:**

```rust
// copybook-core/src/utils.rs

    /// Branch prediction hint for common success cases
    ///
    /// PERFORMANCE OPTIMIZATION: Helps CPU branch predictor optimize hot paths
    #[allow(clippy::inline_always)]
    #[inline(always)]
    #[allow(dead_code)]
    fn likely(condition: bool) -> bool {
        if condition {
            true
        } else {
            // Mark error path as cold to optimize for success case
            cold_path();
            false
        }
    }

    /// Mark error paths as cold for branch prediction optimization
    #[cold]
    #[inline(never)]
    #[allow(dead_code)]
    fn cold_path() {
        // Empty function to hint that this path is unlikely
    }
```

**Rationale for Removal**

- **Code Hygiene**: Unused code should be removed to keep the codebase clean and maintainable.
- **Reduced Confusion**: Removing these functions eliminates potential confusion with the `likely` function in `copybook-codec/src/numeric.rs`, which is actively used for performance optimization.
- **Clarity**: It makes the codebase easier to understand, as developers won't spend time wondering if these functions are used in some non-obvious way.

**Proposed Fix**

The fix is to remove both functions entirely from `copybook-core/src/utils.rs`. They provide no value and only serve to clutter the module.

**Action:**

Delete the `likely` and `cold_path` functions and their associated comments from `copybook-core/src/utils.rs`.

```rust
// copybook-core/src/utils.rs

// ... (previous code in the `safe_ops` module)

    /*
     The following functions should be deleted:
     - fn likely(condition: bool) -> bool { ... }
     - fn cold_path() { ... }
    */

    /// Safely parse string as u16 with context
    ///
    /// Used for COBOL numeric field parsing where invalid digits could cause
    /// parse errors during copybook processing.
    #[inline]
    pub fn safe_parse_u16(s: &str, context: &str) -> Result<u16> {
// ... (rest of the file)
```