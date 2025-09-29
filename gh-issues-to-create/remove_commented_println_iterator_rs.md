---
title: Remove Commented-Out `println!`/`eprintln!` from `copybook-codec/src/iterator.rs` Documentation
labels: ["code-quality", "cleanup", "documentation"]
assignees: []
---

## Issue Description

In `copybook-codec/src/iterator.rs`, lines 34 and 37 contain commented-out `println!` and `eprintln!` statements within documentation comments. Specifically:

*   `///             println!("Record {}: {}", record_index + 1, json_value);`
*   `///             eprintln!("Error in record {}: {}", record_index + 1, error);`

While these lines are commented out and thus not active code, their presence in a library's documentation can be misleading. They might suggest that direct printing to `stdout`/`stderr` is an acceptable practice within the library, which goes against the principle of libraries being silent and using proper logging mechanisms.

## Locations

*   `copybook-codec/src/iterator.rs:34`
*   `copybook-codec/src/iterator.rs:37`

## Proposed Fix

Remove these commented-out `println!` and `eprintln!` statements from the documentation. If examples of how to handle output or errors are necessary for the documentation, they should demonstrate the use of a proper logging framework (e.g., the `log` crate) or be moved to actual executable examples.

## Example

```rust
// Before (simplified context):
// /// Example usage:
// /// let iterator = RecordIterator::new(...);
// /// for (record_index, result) in iterator.enumerate() {
// ///     match result {
// ///         Ok(json_value) => {
// ///             println!("Record {}: {}", record_index + 1, json_value);
// ///         },
// ///         Err(error) => {
// ///             eprintln!("Error in record {}: {}", record_index + 1, error);
// ///         }
// ///     }
// /// }

// After:
// /// Example usage:
// /// let iterator = RecordIterator::new(...);
// /// for (record_index, result) in iterator.enumerate() {
// ///     match result {
// ///         Ok(json_value) => {
// ///             // Use a logging framework or pass output to a handler
// ///             log::info!("Record {}: {}", record_index + 1, json_value);
// ///         },
// ///         Err(error) => {
// ///             log::error!("Error in record {}: {}", record_index + 1, error);
// ///         }
// ///     }
// /// }
```

This cleanup will improve the clarity and best-practice guidance within the library's documentation.
