# [Task]: Refine `dead_code` Allowances in `copybook-gen/src/enterprise.rs`

**Issue Description**

The file `copybook-gen/src/enterprise.rs` has a broad, file-level `#[allow(dead_code)]` attribute. This is a code smell because it can suppress warnings for genuinely unused code, leading to code rot and increased maintenance burden. While this module is a library for generating test fixtures and may have public API components unused within the crate, the allowance should be scoped to only those specific components, not the entire file.

An analysis of the file shows that all private helper methods appear to be in use, which strongly suggests the file-level allowance is hiding a more nuanced situation.

**File and Location:**

`copybook-gen/src/enterprise.rs:6`

**Code Context:**

```rust
// copybook-gen/src/enterprise.rs

//! Enterprise pattern generation for realistic mainframe COBOL copybook fixtures
//!
//! Implements enterprise-grade COBOL copybook generation following mainframe standards
//! and authentic data patterns used in production systems.

#![allow(dead_code, deprecated, clippy::missing_errors_doc)]
#![allow(
    clippy::unused_self,
    clippy::useless_format,
    clippy::module_name_repetitions
)]
// ... rest of the file
```

**Proposed Fix**

The goal is to replace the overly broad, file-level `allow` with specific, targeted `allow` attributes on only the necessary items. This makes the code cleaner and the intention clear.

Here is a step-by-step guide to resolving this issue:

1.  **Remove the File-Level Allowance**: Delete the line `#![allow(dead_code, deprecated, clippy::missing_errors_doc)]` from the top of the file.

2.  **Run a Compiler Check**: Compile the `copybook-gen` crate (e.g., using `cargo check -p copybook-gen`) and collect all the new `dead_code` warnings. The `deprecated` and `missing_errors_doc` warnings should also be addressed, but this issue focuses on `dead_code`.

3.  **Analyze Each Warning**: For each `dead_code` warning reported by the compiler, apply the following logic:

    *   **Is it a private item (e.g., a private function or module)?**
        If so, it is genuinely unused and should be removed. It provides no value and adds to the maintenance load.

    *   **Is it a public API item (e.g., a `pub struct`, `pub enum`, `pub fn`, or a field of a public struct)?**
        If it's a public item, it's likely intended for use by consumers of the `copybook-gen` crate. In this case, the `dead_code` warning is acceptable. Apply a specific, well-justified `allow` attribute directly to that item.

        **Example:** If the compiler warns that `MainframeTarget` is unused:

        ```rust
        // Add a specific allowance with a justification
        #[allow(dead_code)] // This enum is part of the public API for configuration
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum MainframeTarget {
            Ibm390,
            IbmZ,
            Unisys,
            Tandem,
        }
        ```

4.  **Repeat for All Warnings**: Continue this process until all `dead_code` warnings are resolved by either removing the code or adding a specific `allow` attribute.

By following this process, the crate will be safer from code rot, and future developers will have a clearer understanding of which parts of the code are intended as public API versus which are internal implementation details.