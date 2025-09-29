---
title: Remove Debug `println!` from `copybook-codec/src/lib_api.rs`
labels: ["bug", "code-quality", "cleanup"]
assignees: []
---

## Issue Description

In `copybook-codec/src/lib_api.rs` at line 1637, there is a `println!` statement: `println!("Result: {}", serde_json::to_string_pretty(&result).unwrap());`. The `copybook-codec` crate is a library, and libraries should generally avoid printing directly to `stdout` or `stderr`. Such statements are typically debug remnants or examples that have inadvertently made their way into production code, leading to unexpected output when the library is used by other applications.

## Location

*   `copybook-codec/src/lib_api.rs:1637`

## Proposed Fix

1.  **Remove the `println!`:** The most straightforward fix is to simply remove the `println!` statement. If the output was intended for debugging, it should be replaced with a proper logging mechanism (e.g., using the `log` crate and a configurable logger backend).
2.  **Relocate Example Usage:** If this `println!` was meant to demonstrate the output of `serde_json::to_string_pretty`, it should be moved to an example file (`examples/`) or a test file (`tests/`) within the `copybook-codec` crate.

## Example

```rust
// Before:
// let result = some_function_that_returns_a_result();
// println!("Result: {}", serde_json::to_string_pretty(&result).unwrap());

// After (removal):
// let result = some_function_that_returns_a_result();
// // Output handled by the calling application or through a logging framework

// After (if logging is desired, assuming 'log' crate is used):
// use log::debug;
// let result = some_function_that_returns_a_result();
// debug!("Result: {}", serde_json::to_string_pretty(&result).unwrap());
```

Removing this will ensure that the `copybook-codec` library behaves as a silent dependency, only providing output when explicitly configured by the consuming application.
