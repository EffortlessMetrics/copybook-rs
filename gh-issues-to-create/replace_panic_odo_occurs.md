---
title: Replace `panic!` with Graceful Error Handling for ODO Occurs in `copybook-core/src/parser.rs`
labels: ["bug", "error-handling", "stability"]
assignees: []
---

## Issue Description

In `copybook-core/src/parser.rs` at line 1328, a `panic!` macro is used: `panic!("Expected ODO occurs, got {:?}", odo_field.occurs);`. This indicates that the parser encounters an unhandled state related to `ODO occurs` fields that leads to an immediate program crash. While `panic!` can be useful for unrecoverable errors during development, in a production-grade parser, malformed or unexpected input should ideally be handled gracefully by returning an error, allowing the application to recover or report the issue without crashing.

Crashing on unexpected input can lead to a poor user experience and loss of application state.

## Location

*   `copybook-core/src/parser.rs:1328`

## Proposed Fix

1.  **Define a Specific Error Type:** Introduce a new variant in the `ParseError` enum (or a similar error type used by the parser) that specifically captures the `UnexpectedOdoOccurs` condition. This error variant should include relevant context, such as the expected and found values, and potentially the line and column number where the error occurred.
2.  **Return `Result`:** Modify the function containing the `panic!` to return a `Result<T, ParseError>` type. Instead of panicking, return an `Err` variant with the newly defined error.
3.  **Propagate/Handle Error:** Ensure that the calling code correctly propagates or handles this new error type.

## Example (Illustrative - assuming `parse_odo_field` is the function containing the panic)

```rust
// Before (simplified):
// fn parse_odo_field(...) -> OdoField {
//     // ... parsing logic ...
//     if odo_field.occurs.is_none() { // Simplified condition
//         panic!("Expected ODO occurs, got {:?}", odo_field.occurs);
//     }
//     // ...
//     odo_field
// }

// After (conceptual):
// Define a new error variant:
// #[derive(Debug, PartialEq, Eq)]
// pub enum ParseError {
//     UnexpectedOdoOccurs { found: String, line: usize, column: usize },
//     // ... other error variants
// }

// Modify the function to return a Result:
// fn parse_odo_field(...) -> Result<OdoField, ParseError> {
//     // ... parsing logic ...
//     if odo_field.occurs.is_none() { // Simplified condition
//         return Err(ParseError::UnexpectedOdoOccurs {
//             found: format!("{:?}", odo_field.occurs),
//             line: /* current line number */,
//             column: /* current column number */,
//         });
//     }
//     // ...
//     Ok(odo_field)
// }
```

Implementing graceful error handling will significantly improve the robustness and reliability of the `copybook-core` parser.
